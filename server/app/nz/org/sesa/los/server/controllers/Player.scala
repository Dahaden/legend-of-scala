package nz.org.sesa.los.server.controllers

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

object Player extends Controller {
    def view(playerName : String) = Action { request =>
        val rows = DB.withConnection { implicit c =>
            SQL("""SELECT id, name, x, y
                   FROM players
                   WHERE name = {name}""").on(
                "name" -> playerName
            ).apply()
        }.toList

        rows match {
            case Nil => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Sorry, but you, $playerName, are not an adventurer.")
                ))).as("application/json")
            }
            case row::_ => {
                Ok(json.pretty(json.render(
                    ("id"   -> row[Int]("id")) ~
                    ("name" -> row[String]("name")) ~
                    ("x"    -> row[Int]("x")) ~
                    ("y"    -> row[Int]("y"))
                ))).as("application/json")
            }
        }
    }

    def inventory(playerName : String) = Action { request =>
        // TODO: look up inventory in the inventory table
        Ok(json.pretty(json.render(List(
            ("id" -> 1) ~
            ("kind" -> "paper-map"),

            ("id" -> 2) ~
            ("kind" -> "legend"),

            ("id" -> 3) ~
            ("kind" -> "beacon")
        )))).as("application/json")
    }

    def look(playerName : String) = Action { request =>
        // TODO: look at the world map
        Ok(json.pretty(json.render(
            ("synopsis" -> "Things.")
        ))).as("application/json")
    }

    def move(playerName : String) = Action { request =>
        // TODO: actually move
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
