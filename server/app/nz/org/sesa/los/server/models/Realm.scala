package nz.org.sesa.los.server.models

import scala.collection.mutable
import scala.util.control.Breaks._

import anorm._
import net.liftweb.json
import play.api.Play.current
import play.api._
import play.api.db._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import scala.util.Random
import java.io.PrintWriter

object Realm {
    def getRow(realmName : String) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT id, name, w, h
                              FROM realms
                              WHERE name = {name}""").on(
                "name" -> realmName
            )

            rows().toList match {
                case Nil => None
                case row::_ => Some(row)
            }
        }
    }

    private def fileNameForRealm(name : String) = s"maps/${name}.json"

    private val cache : mutable.Map[String, List[Tile]] = mutable.Map().withDefault(loadFromFile)

    case class Tile(val terrain : String, val features : List[String])

    private def loadFromFile(name : String) = {
        implicit val formats = json.DefaultFormats
        json.parse(io.Source.fromFile(this.fileNameForRealm(name)).mkString).extract[List[Tile]]
    }

    def loadTiles(name : String) = {
        this.cache.getOrElseUpdate(name, {
            loadFromFile(name)
        })
    }

    def unloadTiles(name : String) = {
        this.cache -= name
    }

    def generateDungeon(name : String, w : Int, h : Int,
                        minForks : Int = 5, maxForks : Int = 10,
                        minStray : Int = 1, maxStray : Int = 5) = {
        val rand = new Random(System.currentTimeMillis());

        val n = w * h

        val sx = w / 2
        val sy = h - 1

        val dungeon = Array.fill(n) {
            ("terrain" -> "impassable")
        }

        // TODO: maybe entrance?
        dungeon(sy * w + sx) = ("terrain" -> "cave")

        // dig dungeon trunk
        var branchable : List[(Int, Int)] = List()

        var x = sx
        var y = sy

        val sidewayed : Array[Option[(Int, Int)]] = Array.fill(h) { None }

        while (y > 0) {
            branchable = (x, y) +: branchable

            var opts = List((0, -1)) ::: (sidewayed(y) match {
                case None => List((1, 0), (-1, 0))
                case Some(v) => List(v)
            }) filter { case (dx, dy) =>
                x + dx >= 0 && x + dx < w
            }

            val (dx, dy) = opts(rand.nextInt(opts.length))
            if (dy == 0) {
                sidewayed(y) = Some((dx, dy))
            }

            x = x + dx
            y = y + dy

            dungeon(y * w + x) = ("terrain" -> "cave")
        }

        var ex = x
        var ey = y

        // TODO: maybe throne room or something?
        dungeon(ey * w + ex) = ("terrain" -> "cave")

        // dig dungeon forks
        branchable = rand.shuffle(branchable)

        (minForks until Math.max(maxForks, branchable.length)) foreach { _ =>
            var (x, y) = branchable.head
            branchable = branchable.tail

            breakable {
                (minStray until maxStray) foreach { _ =>
                    val opts = List((0, -1), (1, 0), (-1, 0)).filter {case (dx, dy) =>
                        x + dx >= 0 && x + dx < w &&
                        y + dy >= 1 && y + dy < h - 1 &&
                        (dungeon((y + dy) * w + (x + dx)) \ "terrain") == "impassable"
                    }

                    if (opts.length == 0) break

                    val (dx, dy) = opts(rand.nextInt(opts.length))

                    x = x + dx
                    y = y + dy

                    dungeon(y * w + x) = ("terrain" -> "cave")
                }
            }
        }

        // write the dungeon to file
        Some(new PrintWriter(this.fileNameForRealm(name))) foreach {
            p => p.write(json.pretty(json.render(dungeon.toList)))
            p.close
        }

        ((sx, sy), (ex, ey))
    }
}

