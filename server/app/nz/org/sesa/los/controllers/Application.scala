package nz.org.sesa.los.controllers

import nz.org.sesa.los.models.World

import play.api._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

object Application extends Controller {
    val world = World.loadFromFile("world.json")

    def index = Action { request =>
        Ok(json.pretty(json.render(
            ("status" -> "ok")
        ))).as("application/json")
    }

    def map = Action { request =>
        implicit val formats = json.DefaultFormats
        Ok(json.pretty(json.render(decompose(world.tiles)))).as("application/json")
    }
}
