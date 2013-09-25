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

    def getFeatures(realmName : String, x : Int, y : Int) = {
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

    def getMonsters(realmName : String, x : Int, y : Int) = {
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

    def getAdventurers(realmName : String, x : Int, y : Int) = {
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

    private def fileNameForRealm(name : String) = s"maps/${name}.json"

    private val cache : mutable.Map[String, List[Tile]] = mutable.Map().withDefault(loadFromFile)

    case class Tile(val terrain : String)

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

    def generateDungeon(name : String, width : Int, height : Int,
                        minForks : Int = 5, maxForks : Int = 10,
                        minStray : Int = 1, maxStray : Int = 5) = {
        val NORTH = 1
        val SOUTH = 2
        val WEST = 4
        val EAST = 8

        val rand = new Random()

        val wthird = width / 3
        val hthird = height / 3

        def generateDungeonMask(width : Int, height : Int) = {
            var x = width / 2
            var y = height - 1

            var trunk : List[(Int, Int)] = List()

            val n = width * height
            val dungeonMask = Array.fill(n) {0}

            dungeonMask(y * width + x) = SOUTH

            val sidewayed : Array[Option[(Int, Int)]] = Array.fill(n) {None}
            val backwarded = Array.fill(n) {false}

            while (y > 0) {
                trunk = (x, y) +: trunk

                var opts = List((0, -1)) ::: (sidewayed(y) match {
                    case None => List((1, 0), (-1, 0))
                    case Some(v) => List(v)
                }) filter { case (dx, dy) =>
                    x + dx >= 0 && x + dx < width
                }

                val (dx, dy) = opts(rand.nextInt(opts.length))

                val nx = x + dx
                val ny = y + dy

                if ((dx, dy) == (0, -1)) {
                    dungeonMask(ny * width + nx)    |= SOUTH
                    dungeonMask(y * width + x)      |= NORTH
                }

                if ((dx, dy) == (1, 0)) {
                    dungeonMask(ny * width + nx)    |= WEST
                    dungeonMask(y * width + x)      |= EAST
                    sidewayed(y) = Some((1, 0))
                }

                if ((dx, dy) == (-1, 0)) {
                    dungeonMask(ny * width + nx)    |= EAST
                    dungeonMask(y * width + x)      |= WEST
                    sidewayed(y) = Some((-1, 0))
                }


                x = nx
                y = ny
            }

            var branchable = rand.shuffle(trunk.slice(0, trunk.length - 1).filter { case (x, y) =>
                (dungeonMask(y * width + x) & NORTH) != 0
            })

            (minForks until Math.max(maxForks, branchable.length)) foreach { _ =>
                var (x, y) = branchable.head
                branchable = branchable.tail

                breakable {
                    (minStray until maxStray) foreach { _ =>
                        val opts = List((0, -1), (1, 0), (-1, 0)).filter {case (dx, dy) =>
                            x + dx >= 0 && x + dx < width &&
                            y + dy >= 1 && y + dy < height - 1 &&
                            dungeonMask((y + dy) * width + (x + dx)) == 0
                        }

                        if (opts.length == 0) break

                        val (dx, dy) = opts(rand.nextInt(opts.length))

                        val nx = x + dx
                        val ny = y + dy

                        if ((dx, dy) == (0, -1)) {
                            dungeonMask(ny * width + nx)    |= SOUTH
                            dungeonMask(y * width + x)      |= NORTH
                        }

                        if ((dx, dy) == (0, 1)) {
                            dungeonMask(ny * width + nx)    |= NORTH
                            dungeonMask(y * width + x)      |= SOUTH
                        }

                        if ((dx, dy) == (1, 0)) {
                            dungeonMask(ny * width + nx)    |= WEST
                            dungeonMask(y * width + x)      |= EAST
                        }

                        if ((dx, dy) == (-1, 0)) {
                            dungeonMask(ny * width + nx)    |= EAST
                            dungeonMask(y * width + x)      |= WEST
                        }

                        x = nx
                        y = ny
                    }
                }
            }

            (dungeonMask, trunk)
        }

        val (dungeonMask, trunk) = generateDungeonMask(wthird, hthird)

        val n = width * height

        val (msx, msy) = trunk.last
        val sx = msx * 3 + 1
        val sy = msy * 3 + 2

        val (mex, mey) = trunk.head
        val ex = mex * 3 + 1
        val ey = mey * 3 - 1

        val dungeon = Array.fill(n) {
            ("terrain" -> "impassable")
        }

        (0 until hthird) foreach { j =>
            (0 until wthird) foreach { i =>
                val x = i * 3
                val y = j * 3

                val v = dungeonMask(j * wthird + i)

                val n = (v & NORTH) != 0
                val s = (v & SOUTH) != 0
                val e = (v & EAST) != 0
                val w = (v & WEST) != 0

                if (n) {
                    dungeon(y * width + (x + 1)) = ("terrain" -> "cave")
                }

                if (s) {
                    dungeon((y + 2) * width + (x + 1)) = ("terrain" -> "cave")
                }

                if (w) {
                    dungeon((y + 1) * width + x) = ("terrain" -> "cave")
                }

                if (e) {
                    dungeon((y + 1) * width + (x + 2)) = ("terrain" -> "cave")
                }

                if ((n && s && !e && !w) || (!n && !s && e && w)) {
                    dungeon((y + 1) * width + (x + 1)) = ("terrain" -> "cave")
                }
            }
        }

        // TODO: maybe entrance/central chamber?
        dungeon(sy * width + sx) = ("terrain" -> "cave")
        dungeon(ey * width + ex) = ("terrain" -> "cave")

        // write the dungeon to file
        Some(new PrintWriter(this.fileNameForRealm(name))) foreach {
            p => p.write(json.pretty(json.render(dungeon.toList)))
            p.close
        }

        ((sx, sy), (ex, ey))
    }
}

