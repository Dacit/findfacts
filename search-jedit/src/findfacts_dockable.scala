package isabelle.jedit_findfacts

import isabelle.jedit.{Dockable, PIDE, Pretty_Text_Area}
import isabelle.{Command, GUI_Thread, Pretty, Session, XML}

import org.gjt.sp.jedit.View


class Findfacts_Dockable(view: View, position: String) extends Dockable(view, position)
{
  GUI_Thread.require {}

  /* component state -- owned by GUI thread */

  private var current_output: List[XML.Tree] = Nil

  /* pretty text area */

  val pretty_text_area = new Pretty_Text_Area(view)

  override def detach_operation: Option[() => Unit] = pretty_text_area.detach_operation

  private def handle_update(): Unit =
  {
    GUI_Thread.require {}

    for {
      plugin <- Findfacts_Plugin.instance
      (snapshot, output) <- plugin.findfacts.state
    } {
      val new_output = List(XML.Text(output))
      if (new_output != current_output) {
        pretty_text_area.update(snapshot, Command.Results.empty, Pretty.separate(new_output))
        current_output = new_output
      }
    }
  }

  /* panel */

  set_content(pretty_text_area)

  /* main */

  private val main =
    Session.Consumer[Any](getClass.getName) {
      case _ => GUI_Thread.later(handle_update())
    }

  override def init(): Unit = {
    PIDE.session.global_options += main
    PIDE.session.caret_focus += main
  }

  override def exit(): Unit = {
    PIDE.session.global_options -= main
    PIDE.session.caret_focus -= main
  }
}
