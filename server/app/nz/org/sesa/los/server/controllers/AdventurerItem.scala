package nz.org.sesa.los.server.controllers

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

object AdventurerItem extends Controller {
    private def getRow(itemId : Int, owner : String) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT items.id AS id, items.kind AS kind, items.attrs AS attrs, adventurers.name AS owner
                               FROM items, adventurers
                               WHERE items.id = {id} AND
                                     items.owner_id = adventurers.id AND
                                     adventurers.name = {owner}""").on(
                "id" -> itemId,
                "owner" -> owner
            )

            rows().toList match {
                case Nil => None
                case row::_ => Some(row)
            }
        }
    }

    def index(adventurerName : String) = Action { request =>
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT items.id AS id, items.kind AS kind, items.attrs AS attrs, adventurers.name AS owner
                              FROM items, adventurers
                              WHERE items.owner_id = adventurers.id AND
                              adventurers.name = {owner}""").on(
                "owner" -> adventurerName
            )

            Ok(json.pretty(json.render(rows().map { row =>
                ("id" -> row[Int]("id")) ~
                ("kind" -> row[String]("kind")) ~
                ("attrs" -> row[Option[String]]("attrs").getOrElse("")) ~
                ("owner" -> row[String]("owner"))
            }))).as("application/json")
        }
    }

    def view(adventurerName : String, itemId : Int) = Action { request =>
        this.getRow(itemId, adventurerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"You don't have this item anymore.")
                ))).as("application/json")
            }
            case Some(row) => {
                Ok(json.pretty(json.render(
                    ("id" -> row[Int]("id")) ~
                    ("kind" -> row[String]("kind")) ~
                    ("attrs" -> row[Option[String]]("attrs").getOrElse("")) ~
                    ("owner" -> row[String]("owner"))
                ))).as("application/json")
            }
        }
    }
}
