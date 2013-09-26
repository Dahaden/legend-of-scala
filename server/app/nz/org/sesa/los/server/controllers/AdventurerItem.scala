package nz.org.sesa.los.server.controllers

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import nz.org.sesa.los.server.models

object AdventurerItem extends Controller {
    def index(adventurerName : String) = Action { request =>
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT items.id AS id,
                                     items.kind AS kind,
                                     items.attrs AS attrs,
                                     adventurers.name AS owner
                              FROM items, adventurers
                              WHERE items.owner_id = adventurers.id AND
                              adventurers.name = {owner}""").on(
                "owner" -> adventurerName
            )

            Ok(json.pretty(json.render(rows().map { row =>
                ("id" -> row[Int]("id")) ~
                ("kind" -> row[String]("kind")) ~
                ("attrs" -> json.parse(row[Option[String]]("attrs").getOrElse("null"))) ~
                ("owner" -> row[String]("owner"))
            }))).as("application/json")
        }
    }

    def view(adventurerName : String, itemId : Int) = Action { request =>
        models.Adventurer.getItem(itemId, adventurerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"You don't have this item anymore.")
                ))).as("application/json")
            }
            case Some(row) => {
                Ok(json.pretty(json.render(
                    ("id" -> row[Int]("id")) ~
                    ("kind" -> row[String]("kind")) ~
                    ("attrs" -> json.parse(row[Option[String]]("attrs").getOrElse("null"))) ~
                    ("owner" -> row[String]("owner"))
                ))).as("application/json")
            }
        }
    }

    def use(adventurerName : String, itemId : Int) = Action(parse.tolerantText) { request =>
        BadRequest(json.pretty(json.render(
            ("why" -> s"Can't use this item.")
        ))).as("application/json")
    }

    def combine(adventurerName : String) = Action(parse.tolerantText) { request =>
        implicit val formats = json.DefaultFormats

        val parts = json.parse(request.body).extract[Map[String, Int]]

        // ensure all the parts exist
        val items = (for {
            (slot, itemId) <- parts
            row <- models.Adventurer.getItem(itemId, adventurerName)
        } yield (slot -> row)).toMap

        val slots = parts.keySet

        if (slots != items.keySet) {
            BadRequest(json.pretty(json.render(
                ("why" -> s"You try to combine the items, but find that some of them don't exist anymore.")
            ))).as("application/json")
        } else if (items.values.toSet.size != slots.size) {
            BadRequest(json.pretty(json.render(
                ("why" -> s"You try to combine an item... with itself?")
            ))).as("application/json")
        } else {
            BadRequest(json.pretty(json.render(
                ("why" -> s"You try to combine the items, but don't quite manage.")
            ))).as("application/json")
        }

    }

    def separate(adventurerName : String, itemId : Int) = Action { request =>
        BadRequest(json.pretty(json.render(
            ("why" -> s"Can't separate this item.")
        ))).as("application/json")
    }
}
