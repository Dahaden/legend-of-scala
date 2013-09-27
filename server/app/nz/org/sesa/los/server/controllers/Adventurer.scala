package nz.org.sesa.los.server.controllers

import nz.org.sesa.los.server.models
import nz.org.sesa.los.server.util
import nz.org.sesa.los.server.Position

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
            val rows = SQL("""SELECT *
                              FROM adventurers, realms
                              WHERE adventurers.realm_id = realms.id""")

            Ok(json.pretty(json.render(rows().map { row =>
                ("id"   -> row[Int]("adventurers.id")) ~
                ("name" -> row[String]("adventurers.name")) ~
                ("pos" ->
                    ("x"    -> row[Int]("adventurers.x")) ~
                    ("y"    -> row[Int]("adventurers.y")) ~
                    ("realm" -> row[String]("realms.name"))
                ) ~
                ("hearts"-> row[Int]("adventurers.hearts"))
            }))).as("application/json")
        }
    }

    def create = Action(parse.tolerantText) { request =>
        val js = json.parse(request.body)

        implicit val formats = json.DefaultFormats

        val name = (js \ "name").extract[String]
        val token = (js \ "token").extract[String]

        DB.withTransaction { implicit c =>
            val w = 10
            val h = 10

            val dungeonName = "Tutorial Dungeon for " + name

            val dungeonId = SQL("""INSERT INTO realms(name, w, h)
                                   VALUES({dungeonName}, {w}, {h})""").on(
                "dungeonName" -> dungeonName,
                "w" -> w,
                "h" -> h
            ).executeInsert().get

            val ((dsx, dsy), (dex, dey)) = models.Realm.generateDungeon(dungeonName, w, h, 0, 0, 0, 0)

            // make chest at start of dungeon
            SQL("""INSERT INTO features(kind, attrs, x, y, realm_id)
                   VALUES ('remote_only', {attrs}, {x}, {y}, {dungeonId})""").on(
                "attrs" -> json.pretty(json.render(
                    ("behavior" -> "chest") ~
                    ("items" -> List(
                        (
                            ("kind" -> "part") ~
                            ("attrs" -> (
                                ("type" -> "stick")
                            ))
                        ),
                        (
                            ("kind" -> "part") ~
                            ("attrs" -> (
                                ("type" -> "plank")
                            ))
                        )
                    ))
                )),
                "x" -> dsx,
                "y" -> dsy,
                "dungeonId" -> dungeonId
            ).execute()

            // force player to get weapon parts
            SQL("""INSERT INTO monsters(kind, drops, hearts, max_hearts, damage, x, y, realm_id)
                   VALUES ('ogre', '[]', 1, 1, 0, {x}, {y}, {dungeonId})""").on(
                "x" -> dsx,
                "y" -> dsy,
                "dungeonId" -> dungeonId
            ).execute()

            // make boss at end of dungeon
            SQL("""INSERT INTO monsters(kind, drops, hearts, max_hearts, damage, x, y, realm_id)
                   VALUES ('ogre', {drops}, 2, 2, 1, {x}, {y}, {dungeonId})""").on(
                "drops" -> json.pretty(json.render(List(
                    (
                        ("kind" -> "map") ~
                        ("attrs" -> new json.JObject(List()))
                    ),
                    (
                        ("kind" -> "map-legend") ~
                        ("attrs" -> new json.JObject(List()))
                    ),
                    (
                        ("kind" -> "beacon") ~
                        ("attrs" -> new json.JObject(List()))
                    )
                ))),
                "x" -> dex,
                "y" -> (dey + 1),
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
                   VALUES ('remote_only', {attrs}, {x}, {y}, {dungeonId})""").on(
                "attrs" -> json.pretty(json.render(
                    ("behavior" -> "portal") ~


                    ("target" -> (
                        ("realm" -> "world") ~
                        ("x" -> x) ~
                        ("y" -> y)
                    )) ~
                    ("change_spawn" -> true)
                )),
                "x" -> dex,
                "y" -> dey,
                "dungeonId" -> dungeonId
            ).execute()

            // generate a nearby dungeon
            models.Realm.makeRandomDungeonAt("world", x, y)

            // make adventurer
            val id = SQL("""INSERT INTO adventurers(name, token, x, y, realm_id, spawn_x, spawn_y, spawn_realm_id, hearts)
                            VALUES ({name}, {token}, {x}, {y}, {dungeonId}, {x}, {y}, {dungeonId}, 10)
                         """).on(
                "name" -> name,
                "token" -> token,
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
        val auth = util.getBasicAuth(request)

        DB.withTransaction { implicit c =>
            val adventurerOption = for {
                (username, password) <- auth
                row <- models.Adventurer.getAuthRow(username, password)
            } yield row

            models.Adventurer.getRow(adventurerName) match {
                case None => {
                    NotFound(json.pretty(json.render(
                        ("why" -> s"Sorry, but you, $adventurerName, are not an adventurer.")
                    ))).as("application/json")
                }
                case Some(_) if auth.isDefined && !adventurerOption.isDefined => {
                    Unauthorized(json.pretty(json.render(
                        ("why" -> s"Oops, your token was wrong.")
                    ))).as("application/json")
                }
                case Some(row) => {
                    Ok(json.pretty(json.render(
                        ("id"    -> row[Int]("adventurers.id")) ~
                        ("name"  -> row[String]("adventurers.name")) ~
                        ("pos" ->
                            ("x"    -> row[Int]("adventurers.x")) ~
                            ("y"    -> row[Int]("adventurers.y")) ~
                            ("realm" -> row[String]("realms.name"))
                        ) ~
                        ("hearts"-> row[Int]("adventurers.hearts"))
                    ))).as("application/json")
                }
            }
        }
    }

    def move(adventurerName : String) = Action(parse.tolerantText) { request =>
        DB.withTransaction { implicit c =>
            val adventurerOption = for {
                (username, password) <- util.getBasicAuth(request)
                row <- models.Adventurer.getAuthRow(username, password)
            } yield row

            adventurerOption match {
                case None => {
                    NotFound(json.pretty(json.render(
                        ("why" -> s"Sorry, but you, $adventurerName, are not an adventurer.")
                    ))).as("application/json")
                }
                case Some(row) if row[String]("adventurers.name") != adventurerName => {
                    Unauthorized(json.pretty(json.render(
                        ("why" -> s"Can't move someone else.")
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

                            val ox = row[Int]("adventurers.x")
                            val oy = row[Int]("adventurers.y")
                            val w = row[Int]("realms.w")

                            val x = ox + dx
                            val y = oy + dy

                            val realm = row[String]("realms.name")

                            val i = y * w + x
                            val target = models.Realm.loadTiles(realm)(i)

                            models.Adventurer.moveDenialFor(new Position(ox, oy, realm), target) match {
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
}
