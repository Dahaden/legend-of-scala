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
    def name = "handheld beacon"
    def examine = "It's some kind of glowing gem with weird glyphs on it. You can use it to find things on the map."
    def action[T : Manifest](args: Any*) = {
        val m = manifest[T]

        if (m == manifest[List[Beacon.Signal]]) {
            // load the map tiles on first use of the map
            val req = :/(Global.ServerAddress) / "adventurers"
            val json.JArray(js) = json.parse(Await.result(Global.http(req), Duration.Inf).getResponseBody())
            implicit val formats = json.DefaultFormats

            for { adventurer <- js } yield {
                val x = (adventurer \ "x").extract[Int]
                val y = (adventurer \ "y").extract[Int]
                val name = (adventurer \ "name").extract[String]

                new Beacon.Signal(x, y, name, Beacon.Signal.Adventurer)
            }
        } else {
            Display.show("It looks like you can use the beacon to find a List of Signals.")
            throw new Item.OAK()
        }
    }.asInstanceOf[T]
}
