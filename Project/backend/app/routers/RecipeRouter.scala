package routers

import controllers.RecipeController
import javax.inject.Inject
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class RecipeRouter @Inject()(controller: RecipeController) extends SimpleRouter{
  val prefix = "/recipes"
  override def routes: Routes = {
    case GET(p"/" ? q_?"ingr=$ingr" & q_?"from=${ int(from) }" & q_?"to=${ int(to) }") => controller.result(ingr, from, to)
  }
}
