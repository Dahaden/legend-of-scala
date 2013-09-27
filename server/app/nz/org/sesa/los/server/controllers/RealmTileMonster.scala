package nz.org.sesa.los.server.controllers

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import java.sql.Connection

import nz.org.sesa.los.server.util
import nz.org.sesa.los.server.models
import nz.org.sesa.los.server.Position

object RealmTileMonster extends Controller {
    private def getRow(realmName : String, x : Int, y : Int, monsterId : Int) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT *
                              FROM monsters, realms
                              WHERE monsters.realm_id = realms.id AND
                                    realms.name = {name} AND
                                    monsters.x = {x} AND
                                    monsters.y = {y} AND
                                    monsters.id = {monsterId}""").on(
                "name" -> realmName,
                "x" -> x,
                "y" -> y,
                "monsterId" -> monsterId
            )

            rows().toList match {
                case Nil => None
                case row::_ => Some(row)
            }
        }
    }

    def attack(realmName : String, x : Int, y : Int, monsterId : Int) = Action(parse.tolerantText) { request =>
        // TODO: parse weapon_id from json body
        val adventurerOption = for {
            (username, password) <- util.getBasicAuth(request)
            row <- models.Adventurer.getAuthRow(username, password)
        } yield row

        (for {
            adventurer <- adventurerOption
            row <- this.getRow(realmName, x, y, monsterId)
        } yield (adventurer, row)) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Er, that doesn't exist anymore.")
                ))).as("application/json")
            }

            case Some((adventurer, row)) => {
                implicit val formats = json.DefaultFormats

                // TODO: implement combat
                DB.withTransaction { implicit c =>
                    // give monster drops
                    val json.JArray(drops) = json.parse(row[String]("monsters.drops"))

                    for {
                        drop <- drops
                    } SQL("""INSERT INTO items (kind, attrs, owner_id)
                             VALUES ({kind}, {attrs}, {ownerId})""").on(
                        "kind" -> (drop \ "kind").extract[String],
                        "attrs" -> json.pretty(json.render(drop \ "attrs")),
                        "ownerId" -> adventurer[Int]("adventurers.id")
                    ).executeInsert()

                    SQL("""DELETE FROM monsters
                           WHERE id = {id}""").on(
                        "id" -> monsterId
                    ).execute()
                }

                Ok(json.pretty(json.render(
                    ("why" -> s"You slay the monster. You pick up some items that it dropped.")
                ))).as("application/json")
            }
        }
    }
}
