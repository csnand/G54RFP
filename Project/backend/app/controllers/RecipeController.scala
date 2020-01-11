package controllers

import javax.inject.Inject
import play.api.mvc.{AbstractController, ControllerComponents}
import scalaj.http.Http

sealed case class Tokens(appID: String, appKey: String)


class RecipeController @Inject()(cc: ControllerComponents) extends AbstractController(cc){
  def result(name: Option[String], from: Option[Int], to: Option[Int]) = name match {
    case Some(value) => Action {
      Ok(getRecipe(value, from, to))
    }
    case _ => Action {
      Ok("not found")
    }
  }

  private val tokens = Tokens("7cbc2a4b", "4ef7e04dc41f832ba78c00cf5175b64c")
  private def getRecipe(ingredients : String, from: Option[Int], to: Option[Int]) : String = {
    Http("https://api.edamam.com/search")
      .param("q", ingredients)
      .param("app_id", tokens.appID)
      .param("app_key", tokens.appKey)
      .param("from", from match {
        case Some(fromVal) => fromVal.toString
        case _ => "0"
      })
      .param("to", to match {
        case Some(toVal) => toVal.toString
        case _ => "30"
      })
      .asString.body
  }
}
