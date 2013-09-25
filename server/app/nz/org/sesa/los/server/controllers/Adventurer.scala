package nz.org.sesa.los.server.controllers

import nz.org.sesa.los.server.models

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import scala.util.Random

object Adventurer extends Controller {
    def index() = Action { request =>
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
                              WHERE adventurers.realm_id = realms.id""")

            Ok(json.pretty(json.render(rows().map { row =>
                ("id"   -> row[Int]("id")) ~
                ("name" -> row[String]("name")) ~
                ("level"-> row[Int]("level")) ~
                ("pos" ->
                    ("x"    -> row[Int]("x")) ~
                    ("y"    -> row[Int]("y")) ~
                    ("realm" -> row[String]("realm"))
                ) ~
                ("hp"   -> row[Int]("hp")) ~
                ("xp"   -> row[Int]("xp"))
            }))).as("application/json")
        }
    }

    def create = Action(parse.tolerantText) { request =>
        val js = json.parse(request.body)

        implicit val formats = json.DefaultFormats

        val name = (js \ "name").extract[String]

        DB.withTransaction { implicit c =>
            val w = 10
            val h = 10

            val dungeonId = SQL("""INSERT INTO realms(name, w, h)
                                   VALUES("tutorial dungeon for " || {name}, {w}, {h})""").on(
                "name" -> name,
                "w" -> w,
                "h" -> h
            ).executeInsert().get

            val ((dsx, dsy), (dex, dey)) = models.Realm.generateDungeon("tutorial dungeon for " + name, w, h, 0, 0, 0, 0)

            // make chest at start of dungeon
            SQL("""INSERT INTO features(kind, attrs, x, y, realm_id)
                   VALUES ("remote_only", {attrs}, {x}, {y}, {dungeonId})""").on(
                "attrs" -> json.pretty(json.render(
                    ("behavior" -> "chest") ~
                    ("items" -> List(
                        (
                            ("kind" -> "weapon") ~
                            ("attrs" -> (
                                ("material" -> "wood") ~
                                ("class" -> "sword")
                            ))
                        )
                    ))
                )),
                "x" -> dsx,
                "y" -> dsy,
                "dungeonId" -> dungeonId
            ).execute()

            // make boss at end of dungeon
            SQL("""INSERT INTO monsters(kind, level, drops, x, y, realm_id)
                   VALUES ("ogre", 1, {drops}, {x}, {y}, {dungeonId})""").on(
                "drops" -> json.pretty(json.render(List(
                    ("name" -> "map"),
                    ("name" -> "map-legend"),
                    ("name" -> "beacon")
                ))),
                "x" -> dex,
                "y" -> dey,
                "dungeonId" -> dungeonId
            ).execute()

            // make portal at end of dungeon
            val locs = models.Realm.loadTiles("world").zipWithIndex.filter { case (t, _) =>
                t.terrain == "road1" || t.terrain == "road2" || t.terrain == "road3"
            }.map { case (_, i) =>
                // TODO: DON'T HARDCODE THIS!
                (i % 150, i / 150)
            }

            val rand = new Random(System.currentTimeMillis());
            val (x, y) = locs(rand.nextInt(locs.length))

            SQL("""INSERT INTO features(kind, attrs, x, y, realm_id)
                   VALUES ("remote_only", {attrs}, {x}, {y}, {dungeonId})""").on(
                "attrs" -> json.pretty(json.render(
                    ("behavior" -> "portal") ~
                    ("target" -> (
                        ("realm" -> "world") ~
                        ("x" -> x) ~
                        ("y" -> y)
                    ))
                )),
                "x" -> dex,
                "y" -> dey,
                "dungeonId" -> dungeonId
            ).execute()

            // make adventurer
            val id = SQL("""INSERT INTO adventurers(name, level, x, y, realm_id, hp, xp)
                            VALUES ({name}, 1, {x}, {y}, {dungeonId}, 100, 0)
                         """).on(
                "name" -> name,
                "x" -> dsx,
                "y" -> dsy,
                "dungeonId" -> dungeonId
            ).execute()
        }

        Ok(json.pretty(json.render(
            ("why" -> "ok")
        ))).as("application/json")
    }

    def view(adventurerName : String) = Action { request =>
        models.Adventurer.getRow(adventurerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Sorry, but you, $adventurerName, are not an adventurer.")
                ))).as("application/json")
            }
            case Some(row) => {
                Ok(json.pretty(json.render(
                    ("id"    -> row[Int]("id")) ~
                    ("name"  -> row[String]("name")) ~
                    ("level" -> row[Int]("level")) ~
                    ("pos" ->
                        ("x"    -> row[Int]("x")) ~
                        ("y"    -> row[Int]("y")) ~
                        ("realm" -> row[String]("realm"))
                    ) ~
                    ("hp"    -> row[Int]("hp")) ~
                    ("xp"    -> row[Int]("xp"))
                ))).as("application/json")
            }
        }
    }

    def move(adventurerName : String) = Action(parse.tolerantText) { request =>
        models.Adventurer.getRow(adventurerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Sorry, but you, $adventurerName, are not an adventurer.")
                ))).as("application/json")
            }
            case Some(row) => {
                val js = json.parse(request.body)
                implicit val formats = json.DefaultFormats

                val direction = (js \ "direction").extract[String]
                direction match {
                    case "north" | "south" | "east" | "west" |
                         "northwest" | "southwest" | "northeast" | "southeast" => {
                        val (dx, dy) = direction match {
                            case "north" => (0, -1)
                            case "northwest" => (-1, -1)
                            case "south" => (0, 1)
                            case "southwest" => (-1, 1)
                            case "east" => (1, 0)
                            case "northeast" => (1, -1)
                            case "west" => (-1, 0)
                            case "southeast" => (1, 1)
                        }

                        val x = row[Int]("x") + dx
                        val y = row[Int]("y") + dy
                        val w = row[Int]("w")
                        val realm = row[String]("realm")

                        val i = y * w + x
                        val tile = models.Realm.loadTiles(realm)(i)

                        models.Adventurer.moveDenialFor(row, tile) match {
                            case Some(denial) => BadRequest(json.pretty(json.render(
                                ("why" -> denial)
                            ))).as("application/json")

                            case None => {
                                val rows = DB.withConnection { implicit c =>
                                    SQL("""UPDATE adventurers
                                           SET x = {x}, y = {y}
                                           WHERE name = {name}""").on(
                                        "name" -> adventurerName,
                                        "x" -> x,
                                        "y" -> y
                                    ).execute()
                                }

                                Ok(json.pretty(json.render(
                                    ("x" -> x) ~
                                    ("y" -> y) ~
                                    ("realm" -> realm)
                                ))).as("application/json")
                            }
                        }
                    }

                    case direction =>
                        BadRequest(json.pretty(json.render(
                            ("why" -> s"Sorry, but $direction isn't actually a direction.")
                        ))).as("application/json")
                }
            }
        }
    }
}
