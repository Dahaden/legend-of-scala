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

object Player extends Controller {
    private def getPlayerRow(playerName : String) = {
        val rows = DB.withConnection { implicit c =>
            SQL("""SELECT id, name, x, y
                   FROM players
                   WHERE name = {name}""").on(
                "name" -> playerName
            ).apply()
        }.toList

        rows match {
            case Nil => None
            case row::_ => Some(row)
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

        DB.withConnection { implicit c =>
            SQL("""INSERT INTO players(name, x, y)
                   VALUES({name}, {x}, {y})""").on(
                "name" -> name,
                "x" -> x,
                "y" -> y
            ).execute()
        }

        Ok(json.pretty(json.render(
            ("why" -> "ok")
        ))).as("application/json")
    }

    def view(playerName : String) = Action { request =>
        this.getPlayerRow(playerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Sorry, but you, $playerName, are not an adventurer.")
                ))).as("application/json")
            }
            case Some(row) => {
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
        this.getPlayerRow(playerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Sorry, but you, $playerName, are not an adventurer.")
                ))).as("application/json")
            }
            case Some(row) => {
                val x = row[Int]("x")
                val y = row[Int]("y")

                val i = y * World.Stride + x
                val tile = World.world.tiles(i)

                Ok(json.pretty(json.render(
                    ("terrain" -> tile.terrain)
                ))).as("application/json")
            }
        }
    }

    def move(playerName : String) = Action(parse.tolerantText) { request =>
        this.getPlayerRow(playerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Sorry, but you, $playerName, are not an adventurer.")
                ))).as("application/json")
            }
            case Some(row) => {
                val js = json.parse(request.body)
                implicit val formats = json.DefaultFormats

                val direction = (js \ "direction").extract[String]
                direction match {
                    case "north" | "south" | "east" | "west" => {
                        val (dx, dy) = direction match {
                            case "north" => (0, -1)
                            case "south" => (0, 1)
                            case "east" => (1, 0)
                            case "west" => (-1, 0)
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
                                    SQL("""UPDATE players
                                           SET x = {x}, y = {y}
                                           WHERE name = {name}""").on(
                                        "name" -> playerName,
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
