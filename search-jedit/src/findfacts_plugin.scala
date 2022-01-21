package isabelle.jedit_findfacts

import isabelle.{Isabelle_System, Options}
import isabelle.jedit.PIDE
import org.gjt.sp.jedit.msg.PluginUpdate
import org.gjt.sp.jedit.{EBMessage, EBPlugin}


class Findfacts_Plugin extends EBPlugin
{
  val findfacts = new Findfacts_Variable()
  private var shutdown = false

  private def activate(): Unit =
  {
    findfacts.install_handlers()
    Findfacts_Plugin._instance = Some(this)
    PIDE.options += ("export_theory", "true")
  }

  private def deactivate(): Unit =
  {
    shutdown = true
    findfacts.uninstall_handlers()
    Findfacts_Plugin._instance = None
  }

  override def handleMessage(message: EBMessage): Unit =
  {
    message match {
      case _: PluginUpdate =>
        if (PIDE._plugin != null && Findfacts_Plugin.instance.isEmpty && !shutdown) {
          activate()
        } else if (Findfacts_Plugin.instance.isDefined) {
          deactivate()
        }
      case _ =>
    }
  }

  override def stop(): Unit = deactivate()
}

object Findfacts_Plugin
{
  /* plugin instance */

  @volatile private var _instance: Option[Findfacts_Plugin] = None

  def instance: Option[Findfacts_Plugin] = _instance
}
