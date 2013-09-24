package nz.org.sesa.los.server.controllers

import play.api._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

object Application extends Controller {
    def index = Action { request =>
        Ok(json.pretty(json.render(
            ("status" -> "ok")
        ))).as("application/json")
    }
}
