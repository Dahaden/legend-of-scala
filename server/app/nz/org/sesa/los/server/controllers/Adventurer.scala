package nz.org.sesa.los.server.controllers

import nz.org.sesa.los.server.models.World

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import scala.util.Random

object Adventurer extends Controller {
    private def getRow(adventurerName : String) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT id, name, level, x, y, hp, xp
                              FROM adventurers
                              WHERE name = {name}""").on(
                "name" -> adventurerName
            )

            rows().toList match {
                case Nil => None
                case row::_ => Some(row)
            }
        }
    }

    def index() = Action { request =>
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT id, name, level, x, y, hp, xp
                              FROM adventurers""")

            Ok(json.pretty(json.render(rows().map { row =>
                ("id"   -> row[Int]("id")) ~
                ("name" -> row[String]("name")) ~
                ("level"-> row[Int]("level")) ~
                ("x"    -> row[Int]("x")) ~
                ("y"    -> row[Int]("y")) ~
                ("hp"   -> row[Int]("hp")) ~
                ("xp"   -> row[Int]("xp"))
            }))).as("application/json")
        }
    }

    def create = Action(parse.tolerantText) { request =>
        val js = json.parse(request.body)

        implicit val formats = json.DefaultFormats

        val name = (js \ "name").extract[String]

        val locs = World.world.tiles.zipWithIndex.filter { case (t, _) =>
            t.terrain == "road1" || t.terrain == "road2" || t.terrain == "road3"
        }.map { case (_, i) =>
            (i % World.Stride, i / World.Stride)
        }

        val rand = new Random(System.currentTimeMillis());
        val (x, y) = locs(rand.nextInt(locs.length))

        // LOL WHAT ARE TRANSACTIONS
        DB.withConnection { implicit c =>
            val id = SQL("""INSERT INTO adventurers(name, level, x, y, hp, xp)
                            VALUES({name}, 1, {x}, {y}, 100, 0)""").on(
                "name" -> name,
                "x" -> x,
                "y" -> y
            ).executeInsert().get

            List("paper-map", "legend", "beacon").foreach { kind =>
                SQL("""INSERT into items(kind, owner_id)
                     VALUES ({kind}, {id})""").on(
                    "kind" -> kind,
                    "id" -> id
                ).execute()
            }
        }

        Ok(json.pretty(json.render(
            ("why" -> "ok")
        ))).as("application/json")
    }

    def view(adventurerName : String) = Action { request =>
        this.getRow(adventurerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Sorry, but you, $adventurerName, are not an adventurer.")
                ))).as("application/json")
            }
            case Some(row) => {
                Ok(json.pretty(json.render(
                    ("id"   -> row[Int]("id")) ~
                    ("name" -> row[String]("name")) ~
                    ("level"-> row[Int]("level")) ~
                    ("x"    -> row[Int]("x")) ~
                    ("y"    -> row[Int]("y")) ~
                    ("hp"   -> row[Int]("hp")) ~
                    ("xp"   -> row[Int]("xp"))
                ))).as("application/json")
            }
        }
    }

    def look(adventurerName : String) = Action { request =>
        this.getRow(adventurerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Sorry, but you, $adventurerName, are not an adventurer.")
                ))).as("application/json")
            }
            case Some(row) => {
                val x = row[Int]("x")
                val y = row[Int]("y")

                val i = y * World.Stride + x
                val tile = World.world.tiles(i)

                Ok(json.pretty(json.render(
                    ("terrain" -> tile.terrain) ~
                    ("x" -> x) ~
                    ("y" -> y)
                ))).as("application/json")
            }
        }
    }

    def move(adventurerName : String) = Action(parse.tolerantText) { request =>
        this.getRow(adventurerName) match {
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

                        val i = y * World.Stride + x
                        val tile = World.world.tiles(i)

                        tile.terrain match {
                            case "ocean" | "lake" | "river" =>
                                BadRequest(json.pretty(json.render(
                                    ("why" -> "You try to flap your wings like a bird to fly over the water, but fail miserably.")
                                ))).as("application/json")
                            case _ => {
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
                                    ("y" -> y)
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
