/*  Title:      jedit_findfacts/findfacts_dockable.scala
    Author:     Fabian Huch, TU Munich

Panel for FindFacts search.
*/
package isabelle.jedit_findfacts


import isabelle.jedit.*
import isabelle.*

import scala.swing.{Component, Label}

import java.awt.event.KeyEvent
import java.awt.BorderLayout

import de.qaware.findfacts.core
import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.{FieldFilter, FilterQuery}
import de.qaware.findfacts.core.QueryService.ResultList
import de.qaware.findfacts.core.dt.ShortBlock

import org.gjt.sp.jedit.View
import org.gjt.sp.jedit.gui.HistoryTextField


class Findfacts_Dockable(view: View, position: String) extends Dockable(view, position) {
  GUI_Thread.require {}

  /* text area */

  val pretty_text_area = new Pretty_Text_Area(view)
  set_content(pretty_text_area)

  def set_text(snapshot: Document.Snapshot, text: XML.Body): Unit = {
    GUI_Thread.require {}
    pretty_text_area.update(snapshot, Command.Results.empty, text)
  }

  def set_result(snapshot: Document.Snapshot, results: ResultList[ShortBlock]): Unit = {
    val sep = List(XML.elem(Markup.SEPARATOR, Pretty.space), Pretty.fbrk, Pretty.fbrk, Pretty.fbrk)

    def print_result(result: ShortBlock): XML.Tree =
      XML.Elem(
        Markup(Markup.ENTITY, List(
          Markup.DEF_LINE -> result.startLine.toString,
          Markup.DEF_FILE -> result.file)),
        Symbol.decode_yxml(result.srcMarkup))

    val text =
      XML.string(results.count.toString + " results found:") ::: sep :::
        Pretty.separate(results.values.toList.map(print_result), sep)

    set_text(snapshot, text)
  }


  // panel state: indexed snapshot + query
  private var _state: Option[(Document.Version, String)] = None

  override def detach_operation: Option[() => Unit] = pretty_text_area.detach_operation

  private def handle_update(): Unit =
    for {
      snapshot <- PIDE.maybe_snapshot()
      if !snapshot.is_outdated
      plugin <- Findfacts_Plugin.instance
    } plugin.findfacts.indexed match {
      case None => GUI_Thread.later(set_text(snapshot, XML.string(plugin.findfacts.index_message)))
      case Some(state) if _state != Some(state, query.getText) =>
        if (!query.getText.isBlank) search()
        else {
          _state = Some(state, "")
          GUI_Thread.later(set_text(snapshot, XML.string(plugin.findfacts.index_message)))
        }
      case _ =>
    }

  /* controls */

  private def search(): Unit = {
    val q = query.getText
    val f_q = FilterQuery(List(FieldFilter(EtField.SourceCode, core.Term(q))))
    for {
      snapshot <- PIDE.maybe_snapshot()
      if !snapshot.is_outdated
      plugin <- Findfacts_Plugin.instance
      state <- plugin.findfacts.indexed
      findfacts = plugin.findfacts.search_service
      indexes <- findfacts.listIndexes
      index <- indexes.headOption
      result <- findfacts.getResultShortlist(f_q)(index)
    } {
      _state = Some(state, query.getText)
      GUI_Thread.later(set_result(snapshot, result))
    }
  }

  private val query_label = new Label("Query:") {
    tooltip =
      GUI.tooltip_lines(
        "Findfacts search query")
  }

  private val query = new HistoryTextField("findfacts-query") {
    override def processKeyEvent(evt: KeyEvent): Unit = {
      if (evt.getID == KeyEvent.KEY_PRESSED && evt.getKeyCode == KeyEvent.VK_ENTER) handle_update()
      super.processKeyEvent(evt)
    }

    setToolTipText(query_label.tooltip)
    setColumns(30)
  }

  private val apply_query = new GUI.Button("<html><b>Search</b></html>") {
    tooltip = "Search FindFacts"

    override def clicked(): Unit = handle_update()
  }

  private val controls =
    Wrap_Panel(
      List(query_label, Component.wrap(query), apply_query))

  add(controls.peer, BorderLayout.NORTH)


  /* main */

  private val main = Session.Consumer[Any](getClass.getName)(_ => handle_update())

  override def init(): Unit = {
    PIDE.session.global_options += main
    PIDE.session.commands_changed += main
    PIDE.session.finished_theories += main
    PIDE.session.caret_focus += main
  }

  override def exit(): Unit = {
    PIDE.session.global_options -= main
    PIDE.session.commands_changed -= main
    PIDE.session.finished_theories -= main
    PIDE.session.caret_focus -= main
  }
}
