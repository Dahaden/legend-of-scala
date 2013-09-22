package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._
import nz.org.sesa.los.client.items._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._

object Player {
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
        val http = new Http()
        val req = :/(Global.ServerAddress) / "players" / name

        implicit val formats = json.DefaultFormats

        val resp = Await.result(http(req), Duration.Inf)
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
        val http = new Http()
        val req = :/(Global.ServerAddress) / "players" / id.toString / "inventory"

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(http(req), Duration.Inf).getResponseBody()).extract[List[Player.RemoteItemHandle]] map { h =>
            h.kind match {
                case "paper-map" => new PaperMap(h.id)
                case "legend" => new Legend(h.id)
                case "beacon" => new Beacon(h.id)
                case _ => null
            }
        }
    }

    def look = {
        val http = new Http();
        val req = :/(Global.ServerAddress) / "players" / id.toString / "look"

        implicit val formats = json.DefaultFormats

        json.parse(Await.result(http(req), Duration.Inf).getResponseBody()).extract[Player.Vision]
    }

    def move(direction : String) = {
        val http = new Http();
        val req = :/(Global.ServerAddress) / "players" / id.toString / "move" << json.pretty(json.render(
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
