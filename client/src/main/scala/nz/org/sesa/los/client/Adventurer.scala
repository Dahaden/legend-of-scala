package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._

object Adventurer {
    object Vision {
        private val FriendlyTerrainNane = Map(
            "ocean" -> "Ocean",
            "coast" -> "Coast",
            "lakeshore" -> "Lakeshore",
            "lake" -> "Lake",
            "river" -> "River",
            "marsh" -> "Marsh",
            "ice" -> "Ice Field",
            "beach" -> "Beach",
            "road1" -> "Road, Highway",
            "road2" -> "Road, Main",
            "road3" -> "Road",
            "bridge" -> "Bridge",
            "lava" -> "Lava",
            "snow" -> "Snowfield",
            "tundra" -> "Tundra",
            "bare" -> "Barren",
            "scorched" -> "Scorched Land",
            "taiga" -> "Boreal Forest",
            "shrubland" -> "Shrubland",
            "temperate-desert" -> "Desert, Temperate",
            "temperate-rain-forest" -> "Rainforest, Temperate",
            "temperate-deciduous-forest" -> "Deciduous Forest, Temperate",
            "grassland" -> "Grassland",
            "subtropical-desert" -> "Desert, Subtropical",
            "tropical-rain-forest" -> "Rainforest, Tropical",
            "tropical-seasonal-forest" -> "Seasonal Forest, Tropical"
        )

        // TODO: write the flavor text
        private val FlavorText = Map(
            "ocean" -> "It's the ocean. I have no idea how you got here.",
            "coast" -> "Coast.",
            "lakeshore" -> "The waves of the lake gently caress the shoreline. The lake is dyed a deep blue.",
            "lake" -> "You're in a lake. You're probably drowning.",
            "river" -> "You're in a river. How did you get here?",
            "marsh" -> "The ground squelches beneath your feet as you walk around. You can see reeds and rushes dot the landscape, interspersed with low-growing shrubbery.",
            "ice" -> "It's really chilly here. You can see",
            "beach" -> "The ocean laps the sand; the tide ebbing in and out. Your footprints leave marks in the sand, only to be taken away by the waves.",
            "road1" -> "You're standing on a wide open road. You can see it trail off into the distance in both directions.",
            "road2" -> "You're standing on a wide open road. You can see it trail off into the distance in both directions.",
            "road3" -> "You're standing on a wide open road. You can see it trail off into the distance in both directions.",
            "bridge" -> 241,
            "lava" -> "AAH AAH AAH YOU'RE STANDING IN LAVA",
            "snow" -> "Everything around you is a brilliant shade of white as you find yourself in a field of snow, glistening in the sunlight.",
            "tundra" -> "The land is a rusty red, baring witness to the snowy peaks of the formidable mountains in the distance.",
            "bare" -> 102,
            "scorched" -> 240,
            "taiga" -> 108,
            "shrubland" -> 102,
            "temperate-desert" -> 186,
            "temperate-rain-forest" -> 65,
            "temperate-deciduous-forest" -> 65,
            "grassland" -> "Green blades of grass fill the land endlessly as they shake gently in the wind.",
            "subtropical-desert" -> 180,
            "tropical-rain-forest" -> 65,
            "tropical-seasonal-forest" -> 65
        )
    }

    /**
     * A vision, obtained when using me.look.
     */
    case class Vision(val pos : Position, val terrain : String) {
        override def toString = s"""
${Display.StartHilight}${Vision.FriendlyTerrainNane.get(terrain).head} (${pos.x}, ${pos.y})${Display.Reset}
${Vision.FlavorText.get(terrain).head}

${Display.StartHilight}Features of Interest${Display.Reset}
Nothing here.

${Display.StartHilight}Monsters${Display.Reset}
No monsters.

${Display.StartHilight}Other Adventurers${Display.Reset}
Nobody here.

"""
    }

    /**
     * Reference to a remote object. The adventurer should never be concerned with
     * this.
     */
    private case class RemoteItemHandle(id : Int, kind : String, owner : String, attrs : json.JObject)

    private def greet(name : String) {
        println(s"""
${Display.StartHilight}Hello, Adventurer $name, and welcome to...${Display.Reset}
${Display.fg(238)}
:::::::::::::::::::::::::::::::::::::${Display.fg(244)}
 T  H  E    L  E  G  E  N  D    O  F ${Display.fg(250)}
:::::::::::::::::::::::::::::::::::::${Display.fg(196)}
.oPYo. .oPYo.      .oo o          .oo
8      8    8     .P 8 8         .P 8
`Yooo. 8         .P  8 8        .P  8
    `8 8        oPooo8 8       oPooo8
     8 8    8  .P    8 8      .P    8
`YooP' `YooP' .P     8 8oooo .P     8${Display.fg(250)}
:.....::.....:..:::::..........:::::.${Display.fg(244)}
:::::::::::::::::::::::::::::::::::::${Display.fg(238)}
:::::::::::::::::::::::::::::::::::::${Display.Reset}

 * Why don't you start off by `.look`ing around?

 * Or maybe checking your `.inventory`?

 * If you're really adventurous, you can start `.move`ing around to the "north", "south", "east" and "west".
""")
    }

    def login(name : String) = {
        val req = :/(Global.ServerAddress) / "adventurers" / name

        implicit val formats = json.DefaultFormats

        val resp = Await.result(Global.http(req), Duration.Inf)
        var js = json.parse(resp.getResponseBody())

        resp.getStatusCode() match {
            case 200 => {
                // we need to rename some things for case class extraction
                js = js ++ (
                    ("level_" -> js \ "level") ~
                    ("pos_" -> js \ "pos") ~
                    ("hp_" -> js \ "hp") ~
                    ("xp_" -> js \ "xp")
                )

                val adventurer = js.extract[Adventurer]
                greet(adventurer.name)
                adventurer
            }

            case 404 => {
                Display.show((js \ "why").extract[String])
                null
            }
        }
    }
}

case class Adventurer private(private val id : Int, val name : String,
                              private var level_ : Int,
                              private var pos_ : Position,
                              private var hp_ : Int, private var xp_ : Int) {
    def level : Int = level_
    def pos : Position = pos_

    def hp : Int = hp_
    def xp : Int = xp_

    def maxHp : Int = level * 100
    def maxXp : Int = level * 100

    def title : String =
        if (level <= 5) {
            "Wanderer"
        } else if (level <= 10) {
            "Vagrant"
        } else if (level <= 20) {
            "Traveler"
        } else if (level <= 40) {
            "Scout"
        } else if (level <= 80) {
            "Ranger"
        } else {
            "Hero"
        }

    var afterMove = () => {}

    private var seenInventory = false

    def inventory = {
        if (!this.seenInventory) {
            println(s"""${Display.StartHilight}Apparently, you're wearing a backpack. There's some stuff in it.${Display.Reset}

 * You can retrieve things from it by number with `.inventory(i)`.

 * You can examine them with `.examine`.

 * Once you've taken something from it, you can use it with `.use()`.

 * Sometimes using an item needs something else, like a word or another item. You can do `.use(other1, other2)`.

 * Other times, items will tell you things but only if you know how to use them. You can do `.use[Result]()` for these.
 """)
            this.seenInventory = true
        }
        val req = :/(Global.ServerAddress) / "adventurers" / name / "items"

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(Global.http(req), Duration.Inf).getResponseBody()).extract[List[Adventurer.RemoteItemHandle]] map { h =>
            h.kind match {
                case "map"          => new items.Map(h.id, h.attrs, this)
                case "map-legend"   => new items.MapLegend(h.id, h.attrs, this)
                case "beacon"       => new items.Beacon(h.id, h.attrs, this)
            }
        }
    }

    def look = {
        val req = :/(Global.ServerAddress) / "realms" / pos.realm / (pos.x.toString + "," + pos.y.toString)

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(Global.http(req), Duration.Inf).getResponseBody()).extract[Adventurer.Vision]
    }

    def move(direction : String) = {
        val req = :/(Global.ServerAddress) / "adventurers" / name / "move" << json.pretty(json.render(
            "direction" -> direction.toLowerCase
        ))

        implicit val formats = json.DefaultFormats

        val resp = Await.result(Global.http(req), Duration.Inf)
        val js = json.parse(resp.getResponseBody())

        resp.getStatusCode() match {
            case 400 => {
                Display.show((js \ "why").extract[String])
                false
            }
            case 200 => {
                this.pos_ = js.extract[Position]
                Display.show(s"You move ${direction.toLowerCase}wards.")
                this.afterMove()
                true
            }
        }
    }

        override def toString = s"""
${Display.StartHilight}$name, the Level $level $title${Display.Reset}
Currently at (${pos.x}, ${pos.y})

${Display.Bold}${Display.fg(196)}HP:${Display.Reset} $hp/$maxHp
${Display.Bold}${Display.fg(226)}XP:${Display.Reset} $xp/$maxXp
"""
}
