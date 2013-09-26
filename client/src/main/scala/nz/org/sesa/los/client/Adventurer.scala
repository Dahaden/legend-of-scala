package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

import scala.concurrent._
import scala.concurrent.duration._

object Adventurer {
    case class RemoteHandle(val id : Int, val name : String, var pos : Position, var hearts : Int)

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

    case class VisionHandle(val pos : Position, val terrain : String, val exits : List[Boolean], val features : List[Feature.RemoteHandle], val adventurers : List[String], val monsters : List[Monster])

    /**
     * A vision, obtained when using me.look.
     */
    case class Vision(val vh : VisionHandle, bindee : Adventurer) {
        def pos = vh.pos
        def terrain = vh.terrain
        def exits = vh.exits.zip(Vision.Directions)
                .filter({case (canExit, _) => canExit})
                .map({case (_, dir) => dir})

        def features = vh.features.map(_.deserialize(bindee))
        def adventurers = vh.adventurers.filter(_ != bindee.name)
        def monsters = vh.monsters

        override def toString = s"""
${Display.StartHilight}${Vision.FriendlyTerrainNane.getOrElse(terrain, terrain)}${Display.Reset}
${Display.StartHilight}.pos =${Display.Reset} $pos

${Vision.FlavorText.getOrElse(terrain, "???")}

${Display.StartHilight}.exits =${Display.Reset} List(${this.exits.map({dir => s"${Display.fg(34)}$dir${Display.Reset}"}).mkString(", ")})

${Display.StartHilight}.features =${Display.Reset} ${features}

${Display.StartHilight}.monsters =${Display.Reset} ${monsters}

${Display.StartHilight}.adventurers =${Display.Reset} List(${adventurers.map({dir => s"${Display.fg(266)}$dir${Display.Reset}"}).mkString(", ")})
"""
    }

    private def greet(name : String) {
        println(s"""
${Display.StartHilight}Hello, Adventurer $name, and welcome to...${Display.Reset}

${io.Source.fromInputStream(this.getClass.getResourceAsStream("/images/splash.txt")).mkString}
 * Why don't you start off by ${Display.StartHilight}.look${Display.Reset}ing around?

 * Or maybe checking your ${Display.StartHilight}.inventory${Display.Reset}?

 * If you're really adventurous, you can start ${Display.StartHilight}.move${Display.Reset}ing around in a cardinal direction, like ${Display.fg(34)}north${Display.Reset}.
""")
    }

    def login(name : String, token : String) = {
        val adventurer = new Adventurer(name, token)
        if (adventurer.refresh) {
            greet(adventurer.name)
            adventurer
        } else {
            null
        }
    }
}

class Adventurer private(val name : String, val token : String) {
    def pos : Position = rh.pos

    def hearts : Int = rh.hearts
    val maxHearts = 10

    private var rh : Adventurer.RemoteHandle = null

    val http = new Http()

    def refresh : Boolean = {
        val req = (:/(Global.ServerAddress) / "adventurers" / this.name).as_!(name, token)

        implicit val formats = json.DefaultFormats

        val resp = Await.result(this.http(req), Duration.Inf)
        val js = json.parse(resp.getResponseBody())

        resp.getStatusCode() match {
            case 200 => {
                this.rh = js.extract[Adventurer.RemoteHandle]
                true
            }

            case 404 | 401 => {
                Display.show((js \ "why").extract[String])
                false
            }
        }
    }

    private var seenInventory = false

    def inventory = {
        if (!this.seenInventory) {
            println(s"""${Display.StartHilight}Apparently, you're wearing a backpack.${Display.Reset}

 * You can retrieve things from it by number with ${Display.StartHilight}.inventory(i)${Display.Reset}.

 * You can examine them with ${Display.StartHilight}.examine${Display.Reset}.

 * Once you've taken something from it, you can use it with ${Display.StartHilight}.use()${Display.Reset}.

 * Sometimes using an item needs something else, like a word or another item. You can do ${Display.StartHilight}.use(other1, other2)${Display.Reset}.

 * Other times, items will tell you things but only if you know how to use them. You can do ${Display.StartHilight}.use[Result]()${Display.Reset} for these.
 """)
            this.seenInventory = true
        }
        val req = :/(Global.ServerAddress) / "adventurers" / name / "items"

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(this.http(req), Duration.Inf).getResponseBody())
            .extract[List[Item.RemoteHandle]].map(_.deserialize(this))
    }

    def look = {
        // In case we're stale.
        this.refresh

        val req = :/(Global.ServerAddress) / "realms" / pos.realm / (pos.x.toString + "," + pos.y.toString)

        implicit val formats = json.DefaultFormats

        var js = json.parse(Await.result(this.http(req), Duration.Inf).getResponseBody())

        new Adventurer.Vision(js.extract[Adventurer.VisionHandle], this)
    }

    def move(direction : String) = {
        val req = (:/(Global.ServerAddress) / "adventurers" / name / "move" << json.pretty(json.render(
            "direction" -> direction.toLowerCase
        ))).as_!(name, token)

        implicit val formats = json.DefaultFormats

        val resp = Await.result(this.http(req), Duration.Inf)
        val js = json.parse(resp.getResponseBody())

        resp.getStatusCode() match {
            case 400 => {
                Display.show((js \ "why").extract[String])
                false
            }
            case 200 => {
                Display.show(s"You move ${direction.toLowerCase}wards.")
                this.refresh
                true
            }
        }
    }

    def combine(mold : AnyRef) : Option[Item] = {
        val parts = for {
            field <- mold.getClass.getDeclaredFields
        } yield {
            if (field.getType != classOf[Item]) {
                Display.show("You try to fit things that aren't items into your mold, and fail miserably.")
                return None
            }

            field.setAccessible(true)
            field.getName -> field.get(mold).asInstanceOf[Item].id
        }

        val req = (:/(Global.ServerAddress) / "adventurers" / name / "combine" << json.pretty(json.render(
            parts.foldRight (new json.JObject(List())) (_ ~ _)
        ))).as_!(name, token)
        val resp = Await.result(this.http(req), Duration.Inf)

        val js = json.parse(resp.getResponseBody())

        implicit val formats = json.DefaultFormats

        resp.getStatusCode() match {
            case 400 => {
                Display.show((js \ "why").extract[String])
                None
            }
            case 200 => {
                implicit val formats = json.DefaultFormats

                Display.show("You combine the items to make a new item.")
                this.inventory.find {_.id == (js \ "item_id").extract[Int]}
            }
        }
    }

    override def toString = s"""
${Display.StartHilight}.name =${Display.Reset} ${name}
${Display.StartHilight}.hearts =${Display.Reset} ${Display.Bold}${Display.fg(196)}${(0 until this.hearts).map({_ => "♥"}).mkString(" ")}${Display.Reset} ${(this.hearts until this.maxHearts).map({_ => "♡"}).mkString(" ")}
"""
}
