package controllers

import play.api._
import play.api.libs.json.Json
import play.api.mvc._

object Application extends Controller {
    def index = Action { request =>
        Ok(Json.toJson(Map("status" -> "hello!")))
    }
}
