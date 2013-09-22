package nz.org.sesa.los.controllers

import play.api._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

object Player extends Controller {
    def view(playerName : String) = Action { request =>
        Ok(json.pretty(json.render(
            ("name" -> playerName) ~
            ("x" -> 50) ~
            ("y" -> 50)
        ))).as("application/json")
    }

    def inventory(playerName : String) = Action { request =>
        Ok(json.pretty(json.render(List(
            ("handle" -> "paper-map") ~
            ("kind" -> "paper-map"),

            ("handle" -> "legend") ~
            ("kind" -> "legend"),

            ("handle" -> "beacon") ~
            ("kind" -> "beacon")
        )))).as("application/json")
    }

    def look(playerName : String) = Action { request =>
        Ok(json.pretty(json.render(
            ("synopsis" -> "Things.")
        ))).as("application/json")
    }

    def move(playerName : String) = Action { request =>
        if (false) {
            BadRequest(json.pretty(json.render(
                ("why" -> "You try to flap your wings like a bird to fly over the water, but fail miserably.")
            ))).as("application/json")
        } else {
            Ok(json.pretty(json.render(
                ("x" -> 51) ~
                ("y" -> 52)
            ))).as("application/json")
        }
    }
}
