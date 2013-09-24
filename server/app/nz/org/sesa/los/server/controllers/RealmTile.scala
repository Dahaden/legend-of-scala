package nz.org.sesa.los.server.controllers

import anorm._
import play.api._
import play.api.Play.current
import play.api.mvc._
import play.api.db._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

import nz.org.sesa.los.server.models

object RealmTile extends Controller {
    private def getFeatures(realmName : String, x : Int, y : Int) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT features.id AS id,
                                     features.kind AS kind,
                                     features.attrs AS attrs,
                                     features.x AS x,
                                     features.y as y,
                                     realms.name AS realm
                              FROM features, realms
                              WHERE features.realm_id = realms.id AND
                                    realms.name = {name} AND
                                    features.x = {x} AND
                                    features.y = {y}""").on(
                "name" -> realmName,
                "x" -> x,
                "y" -> y
            )

            rows().toList
        }
    }

    private def getMonsters(realmName : String, x : Int, y : Int) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT monsters.id AS id,
                                     monsters.kind AS kind,
                                     monsters.level AS level,
                                     monsters.drops AS drops,
                                     monsters.x AS x,
                                     monsters.y as y,
                                     realms.name AS realm
                              FROM monsters, realms
                              WHERE monsters.realm_id = realms.id AND
                                    realms.name = {name} AND
                                    monsters.x = {x} AND
                                    monsters.y = {y}""").on(
                "name" -> realmName,
                "x" -> x,
                "y" -> y
            )

            rows().toList
        }
    }

    private def getAdventurers(realmName : String, x : Int, y : Int) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT adventurers.id AS id,
                                     adventurers.name AS name,
                                     adventurers.level AS level,
                                     adventurers.x AS x,
                                     adventurers.y AS y,
                                     realms.name AS realm,
                                     adventurers.hp AS hp,
                                     adventurers.xp AS xp
                              FROM adventurers, realms
                              WHERE adventurers.realm_id = realms.id AND
                                    realms.name = {name} AND
                                    adventurers.x = {x} AND
                                    adventurers.y = {y}""").on(
                "name" -> realmName,
                "x" -> x,
                "y" -> y
           )

            rows().toList
        }
    }

    def view(realmName : String, x : Int, y : Int, adventurerName : Option[String]) = Action { request =>
        models.Realm.getRow(realmName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"No such realm.")
                ))).as("application/json")
            }

            case Some(row) => {
                val w = row[Int]("w")
                val h = row[Int]("h")

                val pred = adventurerName.fold { _ : models.Realm.Tile => true } { adventurerName =>
                    models.Adventurer.getRow(adventurerName).fold { _ : models.Realm.Tile => false } { row =>
                        models.Adventurer.canMoveTo(row, _)
                    }
                }

                val exits = List(
                    (0, -1), // north
                    (1, -1), // northeast
                    (1, 0), // east
                    (1, 1), // southeast
                    (0, 1), // south
                    (-1, 1), // southwest
                    (-1, 0), // west
                    (-1, -1) // northwest
                ).map { case (dx, dy) =>
                    val nx = x + dx
                    val ny = y + dy

                    val ni = ny * w + nx

                    if (nx < 0 || nx >= w || ny < 0 || ny >= h) {
                        false
                    } else {
                        pred(models.Realm.loadTiles(realmName)(ni))
                    }
                }

                val i = y * w + x
                val tile = models.Realm.loadTiles(realmName)(i)

                Ok(json.pretty(json.render(
                    ("terrain" -> tile.terrain) ~
                    ("features" -> getFeatures(realmName, x, y).map({ row =>
                        ("id" -> row[Int]("id")) ~
                        ("kind" -> row[String]("kind")) ~
                        ("attrs" -> json.parse(row[Option[String]]("attrs").getOrElse("null")))
                    })) ~
                    ("monsters" -> getMonsters(realmName, x, y).map({ row =>
                        ("id" -> row[Int]("id")) ~
                        ("kind" -> row[String]("kind")) ~
                        ("level" -> row[Int]("level")) ~
                        ("drops" -> json.parse(row[String]("drops")))
                    })) ~
                    ("adventurers" -> getAdventurers(realmName, x, y).map({ row =>
                        row[String]("name")
                    })) ~
                    ("pos" ->
                        ("x" -> x) ~
                        ("y" -> y) ~
                        ("realm" -> realmName)
                    ) ~
                    ("exits" -> exits)
                ))).as("application/json")
            }
        }
    }
}
