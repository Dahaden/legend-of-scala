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
import java.sql.Connection

object Realm {
    def generateDungeonName() = {
        val rand = new Random(System.currentTimeMillis)

        val prefixes = List(
            "Accursed",
            "Ancient",
            "Baneful",
            "Batrachian",
            "Black",
            "Bloodstained",
            "Cold",
            "Dark",
            "Devouring",
            "Diabolical",
            "Ebon",
            "Eldritch",
            "Forbidden",
            "Forgotten",
            "Haunted",
            "Hidden",
            "Lonely",
            "Lost",
            "Malevolent",
            "Misplaced",
            "Nameless",
            "Ophidian",
            "Scarlet",
            "Secret",
            "Shrouded",
            "Squamous",
            "Strange",
            "Tenebrous",
            "Uncanny",
            "Unspeakable",
            "Unvanquishable",
            "Unwholesome",
            "Vanishing",
            "Weird"
        )

        val types = List(
            "Abyss",
            "Catacombs",
            "Caverns",
            "Citadel",
            "City",
            "Cyst",
            "Depths",
            "Dungeons",
            "Fane",
            "Fortress",
            "Halls",
            "Haunts",
            "Isle",
            "Keep",
            "Labyrinth",
            "Manse",
            "Maze",
            "Milieu",
            "Mines",
            "Mountain",
            "Oubliette",
            "Panopticon",
            "Pits",
            "Ruins",
            "Sanctum",
            "Shambles",
            "Temple",
            "Tower",
            "Vault"
        )

        val suffixes = List(
            "the Axolotl",
            "Blood",
            "Bones",
            "Chaos",
            "Curses",
            "the Dead",
            "Death",
            "Demons",
            "Despair",
            "Deviltry",
            "Doom",
            "Evil",
            "Fire",
            "Frost",
            "Gloom",
            "Hells",
            "Horrors",
            "Ichor",
            "Id Insinuation",
            "Iron",
            "Madness",
            "Mirrors",
            "Mists",
            "Monsters",
            "Mystery",
            "Necromancy",
            "Oblivion",
            "Peril",
            "Phantasms",
            "Random Harlots",
            "Secrets",
            "Shadows",
            "Sigils",
            "Skulls",
            "Slaughter",
            "Sorcery",
            "Syzygy",
            "Terror",
            "Torment",
            "Treasure",
            "the Undercity",
            "the Underworld",
            "the Unknown"
        )

        s"${prefixes(rand.nextInt(prefixes.length))} ${types(rand.nextInt(types.length))} of ${suffixes(rand.nextInt(suffixes.length))}"
    }

    def difficultyFor(terrain : String) = {
        val rand = new Random(System.currentTimeMillis)

        terrain match {
            case "cave" => rand.nextInt(3) + 3
            case "beach" | "temperate-rain-forest" | "desert-subtropical" | "grassland" | "tropical-rain-forest" | "coast" => 1
            case "tropical-seasonal-forest" | "temperate-desert" | "taiga" | "marsh" | "temperate-deciduous-forest" | "lakeshore" => 2
            case "bare" | "shrubland" => 3
            case "tundra" | "scorched" => 4
            case "ice" => 5
            case "snow" => 0 // endgame dungeon
            case _ => 0
        }
    }

    def spawnsFor(terrain : String) = {
        val rand = new Random(System.currentTimeMillis)

        terrain match {
            case "cave" => List("ogre", "kobold", "elf")
            case "coast" => List("kobold")
            case "lakeshore" => List("elf")
            case "marsh" => List("ogre", "kobold")
            case "ice" => List("ogre", "kobold", "elf")
            case "beach" => List("kobold")
            case "tundra" => List("ogre")
            case "bare" => List("ogre")
            case "scorched" => List("ogre")
            case "taiga" => List("ogre", "kobold", "elf")
            case "shrubland" => List("ogre", "kobold", "elf")
            case "temperate-desert" => List("kobold")
            case "temperate-rain-forest" => List("elf")
            case "temperate-deciduous-forest" => List("elf")
            case "grassland" => List("kobold", "ogre")
            case "subtropical-desert" => List("kobold")
            case "tropical-rain-forest" => List("elf")
            case "tropical-seasonal-forest" => List("elf")
            case _ => List()
        }
    }

    def dropsFor(difficulty : Int) : List[json.JValue] = {
        val rand = new Random(System.currentTimeMillis)

        (difficulty match {
            case 1 => List(
                (30, ("kind" -> "potion") : json.JValue),
                (20, (("kind" -> "part") ~ ("attrs" -> ("type" -> "ingot"))) : json.JValue)
            )

            case 2 => List(
                (50, ("kind" -> "potion") : json.JValue),
                (40, (("kind" -> "part") ~ ("attrs" -> ("type" -> "ingot"))) : json.JValue)
            )
            case 3 => List(
                (60, ("kind" -> "potion") : json.JValue),
                (30, (("kind" -> "part") ~ ("attrs" -> ("type" -> "ingot"))) : json.JValue),
                (5, (("kind" -> "part") ~ ("attrs" -> ("type" -> "diamond"))) : json.JValue)
            )
            case 4 => List(
                (60, ("kind" -> "potion") : json.JValue),
                (40, (("kind" -> "part") ~ ("attrs" -> ("type" -> "ingot"))) : json.JValue),
                (10, (("kind" -> "part") ~ ("attrs" -> ("type" -> "diamond"))) : json.JValue)
            )
            case 5 => List(
                (70, ("kind" -> "potion") : json.JValue),
                (50, (("kind" -> "part") ~ ("attrs" -> ("type" -> "diamond"))) : json.JValue)
            )
        }).flatMap { case (prob, item) =>
            if (rand.nextInt(100) < prob) {
                List(item)
            } else {
                List()
            }
        }
    }

    def makeMonster(realmName : String, x : Int, y : Int, isBoss : Boolean)(implicit c : Connection) = {
        this.getRow(realmName) match {
            case None => false
            case Some(realm) => {
                val tile = this.loadTiles(realmName)(y * realm[Int]("realms.w") + x)

                val rand = new Random(System.currentTimeMillis)

                val drops = this.dropsFor(this.difficultyFor(tile.terrain))

                val difficulty = Math.ceil(this.difficultyFor(tile.terrain) * (if (isBoss) 1.2 else 1.0)).toInt

                val spawns = this.spawnsFor(tile.terrain)
                val spawn = spawns(rand.nextInt(spawns.length))

                // make boss at end of dungeon
                SQL("""INSERT INTO monsters(kind, drops, hearts, max_hearts, damage, x, y, realm_id)
                       VALUES ({kind}, {drops}, {hearts}, {hearts}, {damage}, {x}, {y}, {realmId})""").on(
                    "kind" -> spawn,
                    "drops" -> json.pretty(json.render(drops)),
                    "x" -> x,
                    "y" -> y,
                    "hearts" -> Math.ceil(difficulty * 1.5).toInt,
                    "damage" -> difficulty,
                    "realmId" -> realm[Int]("realms.id")
                ).execute()

                true
            }
        }
    }

    def makeRandomDungeonAt(realmName : String, x : Int, y : Int,
                            w : Int = 10, h : Int = 10, endgame : Boolean = false,
                            fork : Int = 5, stray : Int = 5)(implicit c : Connection) = {
        this.getRow(realmName) match {
            case None => false
            case Some(realm) => {
                val rand = new Random(System.currentTimeMillis)

                val dungeonName = if (!endgame) {
                    var name = this.generateDungeonName
                    while (SQL("""SELECT * FROM realms WHERE name = {dungeonName}""").on("dungeonName" -> name)().toList.length > 0) {
                        name = this.generateDungeonName
                    }
                    name
                } else {
                    "Endgame Dungeon"
                }

                // create realm
                val dungeonId = SQL("""INSERT INTO realms(name, w, h)
                                       VALUES({dungeonName}, {w}, {h})""").on(
                    "dungeonName" -> dungeonName,
                    "w" -> w,
                    "h" -> h
                ).executeInsert().get

                val ((dsx, dsy), (dex, dey)) = this.generateDungeon(dungeonName, w, h, fork, stray)

                // make entry portal
                val portalId = SQL("""INSERT INTO features(kind, attrs, x, y, realm_id)
                                      VALUES ('remote_only', {attrs}, {x}, {y}, {realmId})""").on(
                    "attrs" -> json.pretty(json.render(
                        ("behavior" -> "portal") ~
                        ("target" -> (
                            ("realm" -> dungeonName) ~
                            ("x" -> dsx) ~
                            ("y" -> dsy)
                        )) ~
                        ("change_spawn" -> false)
                    )),
                    "x" -> x,
                    "y" -> y,
                    "realmId" -> realm[Int]("realms.id")
                ).executeInsert().get

                // create creatures at every tile except (dex, dey)
                val tiles = this.loadTiles(dungeonName)

                tiles.zipWithIndex.filter({ case (tile, i) =>
                    val x = i % w
                    val y = i / w

                    tile.terrain != "impassable" && (x, y) != (dex, dey)
                }).foreach({ case (tile, i) =>
                    val x = i % w
                    val y = i / w

                    this.makeMonster(dungeonName, x, y, false)
                })

                val gems = rand.shuffle(List(
                    (
                        ("kind" -> "part") ~
                        ("attrs" -> (
                            ("type" -> "air gem")
                        ))
                    ),
                    (
                        ("kind" -> "part") ~
                        ("attrs" -> (
                            ("type" -> "water gem")
                        ))
                    ),
                    (
                        ("kind" -> "part") ~
                        ("attrs" -> (
                            ("type" -> "fire gem")
                        ))
                    ),
                    (
                        ("kind" -> "part") ~
                        ("attrs" -> (
                            ("type" -> "earth gem")
                        ))
                    )
                ))

                val attrsRendered = json.pretty(json.render(
                    ("behavior" -> "chest") ~
                    ("items" -> List(gems(0), gems(1), gems(3)))
                ))

                // make two chests at end of dungeon
                SQL("""INSERT INTO features(kind, attrs, x, y, realm_id)
                       VALUES ('remote_only', {attrs}, {x}, {y}, {dungeonId})""").on(
                    "attrs" -> attrsRendered,
                    "x" -> dex,
                    "y" -> dey,
                    "dungeonId" -> dungeonId
                ).execute()

                SQL("""INSERT INTO features(kind, attrs, x, y, realm_id)
                       VALUES ('remote_only', {attrs}, {x}, {y}, {dungeonId})""").on(
                    "attrs" -> attrsRendered,
                    "x" -> dex,
                    "y" -> dey,
                    "dungeonId" -> dungeonId
                ).execute()

                // make dungeon boss
                if (!endgame) {
                    this.makeMonster(dungeonName, dex, dey + 1, true)

                    // make exit portal
                    SQL("""INSERT INTO features(kind, attrs, x, y, realm_id)
                           VALUES ('remote_only', {attrs}, {x}, {y}, {realmId})""").on(
                        "attrs" -> json.pretty(json.render(
                            ("behavior" -> "portal") ~
                            ("target" -> (
                                ("realm" -> realmName) ~
                                ("x" -> x) ~
                                ("y" -> y)
                            )) ~
                            ("linked_portal_id" -> portalId) ~
                            ("change_spawn" -> false)
                        )),
                        "x" -> dex,
                        "y" -> dey,
                        "realmId" -> dungeonId
                    ).executeInsert()
                } else {
                    // make boss at end of dungeon
                    SQL("""INSERT INTO monsters(kind, drops, hearts, max_hearts, damage, x, y, realm_id)
                           VALUES ({kind}, {drops}, {hearts}, {hearts}, {damage}, {x}, {y}, {realmId})""").on(
                        "kind" -> "dragon",
                        "drops" -> "[]",
                        "x" -> dex,
                        "y" -> dey,
                        "hearts" -> 40,
                        "damage" -> 8,
                        "realmId" -> dungeonId
                    ).execute()
                }

                true
            }
        }
    }


    def getRow(realmName : String)(implicit c : Connection) = {
        val rows = SQL("""SELECT *
                          FROM realms
                          WHERE name = {name}""").on(
            "name" -> realmName
        )

        rows().toList match {
            case Nil => None
            case row::_ => Some(row)
        }
    }

    def getFeatures(realmName : String, x : Int, y : Int)(implicit c : Connection) = {
        val rows = SQL("""SELECT *
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

    def getMonsters(realmName : String, x : Int, y : Int)(implicit c : Connection) = {
        val rows = SQL("""SELECT *
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

    def getAdventurers(realmName : String, x : Int, y : Int)(implicit c : Connection) = {
        val rows = SQL("""SELECT *
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
                        fork : Int = 5, stray : Int = 5) = {
        val NORTH = 1
        val SOUTH = 2
        val WEST = 4
        val EAST = 8

        val rand = new Random()

        val wthird = width / 3
        val hthird = height / 3

        def generateDungeonMask(w : Int, h : Int) = {
            var x = w / 2
            var y = h - 1

            var trunk : List[(Int, Int)] = List()

            val n = w * h
            val dungeonMask = Array.fill(n) {0}

            dungeonMask(y * w + x) = SOUTH

            val sidewayed : Array[Option[(Int, Int)]] = Array.fill(n) {None}

            while (y > 0) {
                trunk = (x, y) +: trunk

                var opts = List((0, -1)) ::: (sidewayed(y) match {
                    case None => List((1, 0), (-1, 0))
                    case Some(v) => List(v)
                }) filter { case (dx, dy) =>
                    x + dx >= 0 && x + dx < w
                }

                val (dx, dy) = opts(rand.nextInt(opts.length))

                val nx = x + dx
                val ny = y + dy

                if ((dx, dy) == (0, -1)) {
                    dungeonMask(ny * w + nx)    |= SOUTH
                    dungeonMask(y * w + x)      |= NORTH
                }

                if ((dx, dy) == (1, 0)) {
                    dungeonMask(ny * w + nx)    |= WEST
                    dungeonMask(y * w + x)      |= EAST
                    sidewayed(y) = Some((1, 0))
                }

                if ((dx, dy) == (-1, 0)) {
                    dungeonMask(ny * w + nx)    |= EAST
                    dungeonMask(y * w + x)      |= WEST
                    sidewayed(y) = Some((-1, 0))
                }


                x = nx
                y = ny
            }

            var branchable = rand.shuffle(trunk.slice(0, trunk.length - 1).filter { case (x, y) =>
                (dungeonMask(y * w + x) & NORTH) != 0
            })

            (0 until Math.min(fork, branchable.length)) foreach { _ =>
                var (x, y) = branchable.head
                branchable = branchable.tail

                breakable {
                    (0 until stray) foreach { _ =>
                        val opts = List((0, -1), (1, 0), (-1, 0)).filter {case (dx, dy) =>
                            x + dx >= 0 && x + dx < w &&
                            y + dy >= 1 && y + dy < h - 1 &&
                            dungeonMask((y + dy) * w + (x + dx)) == 0
                        }

                        if (opts.length == 0) break

                        val (dx, dy) = opts(rand.nextInt(opts.length))

                        val nx = x + dx
                        val ny = y + dy

                        if ((dx, dy) == (0, -1)) {
                            dungeonMask(ny * w + nx)    |= SOUTH
                            dungeonMask(y * w + x)      |= NORTH
                        }

                        if ((dx, dy) == (0, 1)) {
                            dungeonMask(ny * w + nx)    |= NORTH
                            dungeonMask(y * w + x)      |= SOUTH
                        }

                        if ((dx, dy) == (1, 0)) {
                            dungeonMask(ny * w + nx)    |= WEST
                            dungeonMask(y * w + x)      |= EAST
                        }

                        if ((dx, dy) == (-1, 0)) {
                            dungeonMask(ny * w + nx)    |= EAST
                            dungeonMask(y * w + x)      |= WEST
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

