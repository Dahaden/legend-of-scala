package nz.org.sesa.los.client.items

import nz.org.sesa.los.client._
import nz.org.sesa.los.client.util._
import nz.org.sesa.los.client.Item

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._

object Beacon {
    case class Signal(val x : Int, val y : Int, val name : String, val kind : String) {
        override def toString = s"$kind $name at ($x, $y)"
    }

    object Signal {
        val Adventurer : String = "adventurer"
    }
}

class Beacon(val id : Int, val owner : String) extends Item {
    def name = "beacon"
    def examine = "It's some kind of glowing gem with weird glyphs on it. You can use it to find things on the map."
    def image = s"""
|    ${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m
|  ${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;15m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;15m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;195m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;195m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;81m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m
|${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;15m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;81m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;38m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;38m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;117m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m
|${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;67m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;255m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;255m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;15m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;15m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;123m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m
|${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;81m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;81m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;123m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;81m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;117m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m
|  ${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;80m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;38m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;31m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;195m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m
|    ${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;74m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;24m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;195m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m
|      ${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;81m  ${27.toChar}[0m${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m
|        ${27.toChar}[1m${27.toChar}[48;5;25m  ${27.toChar}[0m
""".trim.stripMargin
    def action[T : Manifest](args: Any*) = () match {
        case _ if manifest[T] != manifest[List[Beacon.Signal]] => {
            Display.show("It looks like you can use the beacon to find a List of Signals.")
            None
        }

        case _ => {
            val req = :/(Global.ServerAddress) / "adventurers"
            val json.JArray(js) = json.parse(Await.result(Global.http(req), Duration.Inf).getResponseBody())
            implicit val formats = json.DefaultFormats

            Some((for { adventurer <- js } yield {
                val x = (adventurer \ "x").extract[Int]
                val y = (adventurer \ "y").extract[Int]
                val name = (adventurer \ "name").extract[String]

                new Beacon.Signal(x, y, name, Beacon.Signal.Adventurer)
            }).asInstanceOf[T])
        }
    }
}
