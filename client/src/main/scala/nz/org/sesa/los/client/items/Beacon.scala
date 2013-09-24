package nz.org.sesa.los.client.items

import nz.org.sesa.los.client._
import nz.org.sesa.los.client.util._
import nz.org.sesa.los.client.Item
import nz.org.sesa.los.client.Images

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._

object Beacon {
    case class Signal(val pos : Position, val name : String, val kind : String) {
        override def toString = s"$kind $name at (${pos.x}, ${pos.y})"
    }

    object Signal {
        val Adventurer : String = "adventurer"
    }
}

class Beacon(val id : Int, val attrs : json.JObject, val owner : Adventurer) extends Item {
    def name = "beacon"
    def examine = "It's some kind of glowing gem with weird glyphs on it. You can use it to find things on the map."
    def image = Images.Beacon

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
                val pos = (adventurer \ "pos").extract[Position]
                val name = (adventurer \ "name").extract[String]

                new Beacon.Signal(pos, name, Beacon.Signal.Adventurer)
            }).asInstanceOf[T])
        }
    }
}
