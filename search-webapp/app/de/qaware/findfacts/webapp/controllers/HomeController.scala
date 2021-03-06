package de.qaware.findfacts.webapp.controllers

import controllers.AssetsFinder
import play.api.Configuration
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Request}

import de.qaware.findfacts.webapp.views

/**
 * This controller delivers the application's home page, where the elm SPA is embedded.
 *
 * @param cc components of this controller
 */
class HomeController(cc: ControllerComponents, assetsFinder: AssetsFinder, config: Configuration)
  extends AbstractController(cc) {

  /**
   * Deliver application.
   *
   * @return index action
   */
  def index(): Action[AnyContent] =
    Action { implicit request: Request[AnyContent] =>
      Ok(views.html.index(assetsFinder))
    }

  /**
   * Returns the application version.
   *
   * @return version action
   */
  def version(): Action[AnyContent] =
    Action { implicit request: Request[AnyContent] =>
      Ok(config.get[String]("app.version"))
    }

  /**
   * Redirects to swagger webjar index.
   *
   * @return redirect to swagger doc
   */
  def redirectDocs(): Action[AnyContent] =
    Action { implicit request: Request[AnyContent] =>
      Redirect("docs/index.html", Map("url" -> Seq("/swagger.json")))
    }
}
