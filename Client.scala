import dispatch._, Defaults._
import net.liftweb.json
import java.net.URLEncoder
import scala.concurrent._
import scala.concurrent.duration._

trait Item {
    val handle : String

    def name : String
    def examine : String
    def use(args: Object*) : Object

    override def toString = s"<Item: $name>"
}

private object PaperMap {
    val Stride = 150

    class Tile(terrain : String, features : List[String]) {

    }

    // cache tiles (we're never going to need to update this once we have them)
    var tiles : List[Tile] = null
}

private class PaperMap(val handle : String) extends Item {
    def name = "paper map"
    def examine = "It's a map, but the legend is missing..."
    def use(args: Object*) = {
        if (PaperMap.tiles == null) {
            // load the map tiles on first use of the map
            val http = new Http();
            val req = :/(Player.ServerAddress) / "map"
            val json.JArray(js) = json.parse(Await.result(http(req), Duration.Inf).getResponseBody())

            implicit val formats = json.DefaultFormats

            PaperMap.tiles = for { tile <- js } yield tile.extract [PaperMap.Tile]
        }
        PaperMap.tiles
    }
}

private object Legend {
    val _Reset = 27.toChar + "[0m"
    val _Fg = 27.toChar + "[48;5;"
    val _Bg = 27.toChar + "[38;5;"

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
    def use(args: Object*) = {
        val s = args(0).asInstanceOf[String]
        s match {
            case "-right-edge" => "\n"

            case _ => {
                val c = Legend.Colors.getOrElse(s, 0)
                Legend._Fg + c.toString + "m" +
                Legend._Bg + c.toString + "m" + "  " +
                Legend._Reset
            }
        }
    }
}

object Markers {
    private val _Reset = 27.toChar + "[0m"
    private val _Fg = 27.toChar + "[48;5;"
    private val _Bg = 27.toChar + "[38;5;"

    val Me = _Fg + "226m" + 27.toChar + _Bg + "0m" + "u!" + _Reset
}

object Features {
    val Dungeon = "dungeon"
}

object Player {
    val ServerAddress = System.getenv("LOS_HOST")
}

class Player(name : String) {
    private def greet() {
        println(s"""${27.toChar}[1m${27.toChar}[38;5;255m
Hello, Adventurer $name, and welcome to...
${27.toChar}[38;5;238m
:::::::::::::::::::::::::::::::::::::${27.toChar}[38;5;244m
 T  H  E    L  E  G  E  N  D    O  F ${27.toChar}[38;5;250m
:::::::::::::::::::::::::::::::::::::${27.toChar}[38;5;196m
.oPYo. .oPYo.      .oo o          .oo
8      8    8     .P 8 8         .P 8
`Yooo. 8         .P  8 8        .P  8
    `8 8        oPooo8 8       oPooo8
     8 8    8  .P    8 8      .P    8
`YooP' `YooP' .P     8 8oooo .P     8${27.toChar}[38;5;250m
:.....::.....:..:::::..........:::::.${27.toChar}[38;5;244m
:::::::::::::::::::::::::::::::::::::${27.toChar}[38;5;238m
:::::::::::::::::::::::::::::::::::::${27.toChar}[0m
""")
    }

    val http = new Http()
    val req = :/(Player.ServerAddress) / "players" / name
    Await.result(http(req), Duration.Inf).getResponseBody()

    greet()

    /**
     * Reference to a remote object.
     */
    private case class RemoteItemHandle(handle : String, kind : String)

    def inventory = {
        val http = new Http()
        val req = :/(Player.ServerAddress) / "players" / name / "inventory"
        val json.JArray(ris) = json.parse(Await.result(http(req), Duration.Inf).getResponseBody())

        implicit val formats = json.DefaultFormats

        ris.map { ri =>
            val h = ri.extract [RemoteItemHandle]
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
        val js = json.parse(Await.result(http(req), Duration.Inf).getResponseBody())
    }

    def move(direction : String) = {
    }

    override def toString = s"<Player: $name>"
}
