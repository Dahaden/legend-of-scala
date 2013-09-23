package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._
import nz.org.sesa.los.client.items._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._

object Player {
    private val FriendlyTerrainNane = Map(
        "ocean" -> "Ocean",
        "coast" -> "Coast",
        "lakeshore" -> "Lakeshore",
        "lake" -> "Lake",
        "river" -> "River",
        "marsh" -> "Marsh",
        "ice" -> "Ice",
        "beach" -> "Beach",
        "road1" -> "Road",
        "road2" -> "Road",
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
        "lakeshore" -> "You're on the shore of a calm lake.",
        "lake" -> "You're in a lake. You're probably drowning.",
        "river" -> "You're in a river. How did you get here?",
        "marsh" -> 240,
        "ice" -> 123,
        "beach" -> 138,
        "road1" -> 235,
        "road2" -> 237,
        "road3" -> 239,
        "bridge" -> 241,
        "lava" -> "AAH AAH AAH YOU'RE STANDING IN LAVA",
        "snow" -> 15,
        "tundra" -> 249,
        "bare" -> 102,
        "scorched" -> 240,
        "taiga" -> 108,
        "shrubland" -> 102,
        "temperate-desert" -> 186,
        "temperate-rain-forest" -> 65,
        "temperate-deciduous-forest" -> 65,
        "grassland" -> "You're on a wide open plain of grass.",
        "subtropical-desert" -> 180,
        "tropical-rain-forest" -> 65,
        "tropical-seasonal-forest" -> 65
    )

    /**
     * A vision, obtained when using me.look.
     */
    case class Vision(val x : Int, val y : Int, val terrain : String) {
        override def toString = s"""${Display.StartHilight}${FriendlyTerrainNane.get(terrain).head} (${x},${y})${Display.Reset}
${FlavorText.get(terrain).head}

${Display.StartHilight}Things of interest${Display.Reset}
Nothing here.
"""
    }

    /**
     * Reference to a remote object. The player should never be concerned with
     * this.
     */
    private case class RemoteItemHandle(id : Int, kind : String)

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
""")
    }

    def login(name : String) = {
        val req = :/(Global.ServerAddress) / "players" / name

        implicit val formats = json.DefaultFormats

        val resp = Await.result(Global.http(req), Duration.Inf)
        var js = json.parse(resp.getResponseBody())

        resp.getStatusCode() match {
            case 200 => {
                // we need to rename x and y to x_ and y_ so we can construct the
                // player case class
                val x = js \ "x"
                val y = js \ "y"

                js = js ++ (("x_" -> x) ~ ("y_" -> y))

                val player = js.extract[Player]
                greet(player.name)
                player
            }

            case 404 => {
                Display.show((js \ "why").extract[String])
                null
            }
        }
    }
}

case class Player private(private val id : Int, val name : String, private var x_ : Int, private var y_ : Int) {
    def x : Int = x_
    def y : Int = y_

    var afterMove = () => {}

    def inventory = {
        val req = :/(Global.ServerAddress) / "players" / name / "inventory"

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(Global.http(req), Duration.Inf).getResponseBody()).extract[List[Player.RemoteItemHandle]] map { h =>
            h.kind match {
                case "paper-map" => new PaperMap(h.id)
                case "legend" => new Legend(h.id)
                case "beacon" => new Beacon(h.id)
                case _ => null
            }
        }
    }

    def look = {
        val req = :/(Global.ServerAddress) / "players" / name / "look"

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(Global.http(req), Duration.Inf).getResponseBody()).extract[Player.Vision]
    }

    def move(direction : String) = {
        val req = :/(Global.ServerAddress) / "players" / name / "move" << json.pretty(json.render(
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
                this.x_ = (js \ "x").extract[Int]
                this.y_ = (js \ "y").extract[Int]
                Display.show(s"You move ${direction.toLowerCase}wards.")
                this.afterMove()
                true
            }
        }
    }

    override def toString = s"<Player: $name>"
}
