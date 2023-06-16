package isabelle.jedit_findfacts

import java.awt.BorderLayout

import scala.swing.{Component, Label}

import de.qaware.findfacts.core
import de.qaware.findfacts.common.dt.EtField
import de.qaware.findfacts.core.{FieldFilter, FilterQuery}
import isabelle.jedit.*
import isabelle.*
import org.gjt.sp.jedit.View
import org.gjt.sp.jedit.gui.HistoryTextField
import java.awt.event.KeyEvent



class Findfacts_Dockable(view: View, position: String) extends Dockable(view, position) {
  GUI_Thread.require {}

  /* text area */

  val pretty_text_area = new Pretty_Text_Area(view)
  set_content(pretty_text_area)

  // panel state: indexed snapshot + query
  private var _state: Option[(Document.Version, String)] = None

  override def detach_operation: Option[() => Unit] = pretty_text_area.detach_operation

  private def handle_update(): Unit = {
    GUI_Thread.require {}

    for {
      snapshot <- PIDE.maybe_snapshot()
      if !snapshot.is_outdated
      plugin <- Findfacts_Plugin.instance
      state <- plugin.findfacts.indexed
      if _state != Some(state, query.getText)
    } {
      if (!query.getText.isBlank) search()
      else {
        val text = "Updated to " + snapshot.version.id.toString
        pretty_text_area.update(snapshot, Command.Results.empty, Pretty.separate(List(XML.Text(text))))
        _state = Some(state, "")
      }
    }
  }

  /* controls */

  private def search(): Unit = {
    val q = query.getText
    val f_q = FilterQuery(List(FieldFilter(EtField.Name, core.Term(q))))
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
      val text =
        "Indexes: " + commas_quote(indexes) + "\n" +
        "Results for " + quote(q) + ": " + result.count + "\n"
      pretty_text_area.update(snapshot, Command.Results.empty, Pretty.separate(List(XML.Text(text))))
      _state = Some(state, query.getText)
    }
  }

  private val query_label = new Label("Query:") {
    tooltip =
      GUI.tooltip_lines(
        "Findfacts search query")
  }

  private val query = new HistoryTextField("findfacts-query") {
    override def processKeyEvent(evt: KeyEvent): Unit = {
      if (evt.getID == KeyEvent.KEY_PRESSED && evt.getKeyCode == KeyEvent.VK_ENTER) search()
      super.processKeyEvent(evt)
    }

    setToolTipText(query_label.tooltip)
    setColumns(30)
  }

  private val apply_query = new GUI.Button("<html><b>Search</b></html>") {
    tooltip = "Search FindFacts"

    override def clicked(): Unit = search()
  }

  private val controls =
    Wrap_Panel(
      List(query_label, Component.wrap(query), apply_query))

  add(controls.peer, BorderLayout.NORTH)


  /* main */

  private val main = Session.Consumer[Any](getClass.getName)(_ => GUI_Thread.later(handle_update()))

  override def init(): Unit = {
    PIDE.session.global_options += main
    PIDE.session.commands_changed += main
    PIDE.session.finished_theories += main
  }

  override def exit(): Unit = {
    PIDE.session.global_options -= main
    PIDE.session.commands_changed -= main
    PIDE.session.finished_theories -= main
  }
}
