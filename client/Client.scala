import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._

private object Display {
    val Reset = 27.toChar + "[0m"
    val Bold = 27.toChar + "[1m"
    def fg(c: Int) = 27.toChar + s"[38;5;${c}m"
    def bg(c: Int) = 27.toChar + s"[48;5;${c}m"

    val StartHilight = s"${fg(255)}$Bold"
    def show(x: String) = println(s"""${Display.StartHilight}$x${Display.Reset}
""")
}

trait Item {
    val handle : String

    def rejectUse() {
        Display.show(s"You fumble with the $name but can't quite figure out how to use it.")
    }

    def name : String
    def examine : String
    def use(args: Any*) : Any

    override def toString = {
        (this.name.charAt(0) match {
            case 'a' | 'e' | 'i' | 'o' | 'u' => "an"
            case _ => "a"
        }) + " " + this.name
    }
}

class Tile(val x : Int, val y : Int, val terrain : String, val features : List[String]) {
    override def toString = s"$terrain tile at ($x,$y)"
}

private object PaperMap {
    private val Stride = 150

    // cache tiles (we're never going to need to update this once we have them)
    private var tiles : List[Tile] = null
}

private class PaperMap(val handle : String) extends Item {
    def name = "paper map"
    def examine = "It's a map, but the legend is missing..."
    def use(args: Any*) : Any = {
        if (args.length != 0) {
            this.rejectUse()
            return null
        }

        if (PaperMap.tiles == null) {
            // load the map tiles on first use of the map
            val http = new Http();
            val req = :/(Player.ServerAddress) / "map"
            val json.JArray(js) = json.parse(Await.result(http(req), Duration.Inf).getResponseBody())

            implicit val formats = json.DefaultFormats

            PaperMap.tiles = for { (tile, i) <- js.zipWithIndex } yield {
                val x = i % PaperMap.Stride
                val y = i / PaperMap.Stride

                (tile ++ (("x" -> x) ~ ("y" -> y))).extract[Tile]
            }
        }
        PaperMap.tiles
    }
}

private object Legend {
    val Colors = Map(
        "ocean" -> 60,
        "coast" -> 238,
        "lakeshore" -> 24,
        "lake" -> 60,
        "river" -> 24,
        "marsh" -> 240,
        "ice" -> 123,
        "beach" -> 138,
        "road1" -> 235,
        "road2" -> 237,
        "road3" -> 239,
        "bridge" -> 241,
        "lava" -> 167,
        "snow" -> 15,
        "tundra" -> 249,
        "bare" -> 102,
        "scorched" -> 240,
        "taiga" -> 108,
        "shrubland" -> 102,
        "temperate-desert" -> 186,
        "temperate-rain-forest" -> 65,
        "temperate-deciduous-forest" -> 65,
        "grassland" -> 107,
        "subtropical-desert" -> 180,
        "tropical-rain-forest" -> 65,
        "tropical-seasonal-forest" -> 65
    )
}

private class Legend(val handle: String) extends Item {
    def name = "map legend"
    def examine = "It's a torn off piece of paper, with some kind of map legend on it."
    def use(args: Any*) : Any = {
        if (args.length != 1) {
            this.rejectUse()
            return null
        }

        val s = args(0).asInstanceOf[String]
        val c = Legend.Colors.getOrElse(s, 0)

        Display.fg(c) + Display.bg(c) + "  " + Display.Reset
    }
}

object Markers {
    val Me = Display.Bold + Display.fg(0) + Display.bg(226) + "u!" + Display.Reset
}

object Features {
    val Dungeon = "dungeon"
}

object Player {
    val ServerAddress = System.getenv("LOS_HOST")

    /**
     * A vision, obtained when using me.look.
     */
    case class Vision(val synopsis : String) {
        override def toString = s"""
${Display.StartHilight}You can see:${Display.Reset}
$synopsis

${Display.StartHilight}Things of interest:${Display.Reset}
Nothing here.
"""
    }

    /**
     * Reference to a remote object. The player should never be concerned with
     * this.
     */
    private case class RemoteItemHandle(handle : String, kind : String)

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
        val http = new Http()
        val req = :/(Player.ServerAddress) / "players" / name

        implicit val formats = json.DefaultFormats

        var js = json.parse(Await.result(http(req), Duration.Inf).getResponseBody())

        // we need to rename x and y to x_ and y_ so we can construct the
        // player case class
        val x = js \ "x"
        val y = js \ "y"

        js = js ++ (("x_" -> x) ~ ("y_" -> y))

        val player = js.extract[Player]
        greet(player.name)
        player
    }
}

case class Player private(val name : String, private var x_ : Int, private var y_ : Int) {
    def x : Int = x_
    def y : Int = y_

    var afterMove = () => {}

    def inventory = {
        val http = new Http()
        val req = :/(Player.ServerAddress) / "players" / name / "inventory"

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(http(req), Duration.Inf).getResponseBody()).extract[List[Player.RemoteItemHandle]] map { h =>
            h.kind match {
                case "paper-map" => new PaperMap(h.handle)
                case "legend" => new Legend(h.handle)
                case _ => null
            }
        }
    }

    def look = {
        val http = new Http();
        val req = :/(Player.ServerAddress) / "players" / name / "look"

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(http(req), Duration.Inf).getResponseBody()).extract[Player.Vision]
    }

    def move(direction : String) = {
        val http = new Http();
        val req = :/(Player.ServerAddress) / "players" / name / "move" << json.pretty(json.render(
            "direction" -> direction
        ))

        implicit val formats = json.DefaultFormats

        val resp = Await.result(http(req), Duration.Inf)
        val js = json.parse(resp.getResponseBody())

        resp.getStatusCode() match {
            case 400 => {
                Display.show((js \ "why").extract[String])
            }
            case 200 => {
                this.x_ = (js \ "x").extract[Int]
                this.y_ = (js \ "y").extract[Int]
                Display.show(s"You move ${direction}wards.")
                this.afterMove()
            }
        }
    }

    override def toString = s"<Player: $name>"
}
