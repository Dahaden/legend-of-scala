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
            "cave" -> "Cave",
            "impassable" -> "Solid Wall",
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
            "cave" -> "You're in some kind of cave system. It's pretty dark in here.",
            "impassable" -> "Wat.",
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

        val Directions = List("north", "northeast", "east", "southeast", "south", "southwest", "west", "northwest")
    }

    /**
     * A vision, obtained when using me.look.
     */
    case class Vision(val pos : Position, val terrain : String, val exits : List[Boolean], val features_ : List[Feature.RemoteHandle]) {
        val features = features_.map(_.deserialize)

        override def toString = {
            val directions = exits.zip(Vision.Directions)
                .filter({case (canExit, _) => canExit})
                .map({case (_, dir) => s"${Display.fg(34)}$dir${Display.Reset}"})

            val possibleExists = directions.length match {
                case 0 => "You're trapped!"
                case 1 => s"You can only move ${directions(0)} from here."
                case _ => s"You can move ${directions.init.mkString(", ")} or ${directions.last} from here."
            }

            s"""
${Display.StartHilight}${Vision.FriendlyTerrainNane.get(terrain).head} (${pos.x}, ${pos.y})${Display.Reset}
${Vision.FlavorText.get(terrain).head}

$possibleExists

${Display.StartHilight}Features of Interest${Display.Reset}
${features}

${Display.StartHilight}Monsters${Display.Reset}
No monsters.

${Display.StartHilight}Other Adventurers${Display.Reset}
Nobody here.

"""
        }
    }

    private def greet(name : String) {
        println(s"""
${Display.StartHilight}Hello, Adventurer $name, and welcome to...${Display.Reset}

${Images.LogoSplash}
 * Why don't you start off by ${Display.fg(34)}.look${Display.Reset}ing around?

 * Or maybe checking your ${Display.fg(34)}.inventory${Display.Reset}?

 * If you're really adventurous, you can start ${Display.fg(34)}.move${Display.Reset}ing around in a cardinal direction, like ${Display.fg(34)}north${Display.Reset}.
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
                              private var hp_ : Int,
                              private var xp_ : Int) {
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

    private var seenInventory = false

    def inventory = {
        if (!this.seenInventory) {
            println(s"""${Display.StartHilight}Apparently, you're wearing a backpack. There's some stuff in it.${Display.Reset}

 * You can retrieve things from it by number with ${Display.fg(34)}.inventory(i)${Display.Reset}.

 * You can examine them with ${Display.fg(34)}.examine${Display.Reset}.

 * Once you've taken something from it, you can use it with ${Display.fg(34)}.use()${Display.Reset}.

 * Sometimes using an item needs something else, like a word or another item. You can do ${Display.fg(34)}.use(other1, other2)${Display.Reset}.

 * Other times, items will tell you things but only if you know how to use them. You can do ${Display.fg(34)}.use[Result]()${Display.Reset} for these.
 """)
            this.seenInventory = true
        }
        val req = :/(Global.ServerAddress) / "adventurers" / name / "items"

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(Global.http(req), Duration.Inf).getResponseBody())
            .extract[List[Item.RemoteHandle]].map(_.deserialize(this))
    }

    def look = {
        val req = :/(Global.ServerAddress) / "realms" / pos.realm / (pos.x.toString + "," + pos.y.toString) <<? Map("adventurerName" -> name)

        implicit val formats = json.DefaultFormats

        var js = json.parse(Await.result(Global.http(req), Duration.Inf).getResponseBody())
        js = js ++ (
            ("features_" -> js \ "features")
        )

        js.extract[Adventurer.Vision]
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
                true
            }
        }
    }

        override def toString = s"""
${Display.StartHilight}$name, the Level $level $title${Display.Reset}
${Display.Bold}${Display.fg(196)}HP:${Display.Reset} $hp/$maxHp
${Display.Bold}${Display.fg(226)}XP:${Display.Reset} $xp/$maxXp
"""
}
