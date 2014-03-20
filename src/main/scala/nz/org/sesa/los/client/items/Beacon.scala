package nz.org.sesa.los.client.items

import nz.org.sesa.los.client._
import nz.org.sesa.los.client.util._
import nz.org.sesa.los.client.Item

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe.{TypeTag, typeOf}

object Beacon {
    case class Signal(val pos : Position, val name : String) {
        override def toString = s"Signal(.pos = $pos, .name = $name)"
    }
}

class Beacon(val id : Int, val owner : Adventurer) extends Item {
    def name = "beacon"
    def examine = "It's some kind of glowing gem with weird glyphs on it. You can use it to find things on the map."
    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream("/images/beacon.txt")).mkString

    def action[T : TypeTag](args: Any*) = () match {
        case _ if !(typeOf[T] =:= typeOf[List[Beacon.Signal]]) => {
            Display.show("It looks like you can use the beacon to find a List of Signals.")
            None
        }

        case _ => {
            val req = :/(Global.ServerAddress) / "adventurers"
            val json.JArray(js) = json.parse(Await.result(owner.http(req), Duration.Inf).getResponseBody())
            implicit val formats = json.DefaultFormats

            Some((for { adventurer <- js } yield {
                val pos = (adventurer \ "pos").extract[Position]
                val name = (adventurer \ "name").extract[String]

                new Beacon.Signal(pos, name)
            }).asInstanceOf[T])
        }
    }
}
