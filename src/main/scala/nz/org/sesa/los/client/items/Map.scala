package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.Adventurer
import nz.org.sesa.los.client.Global
import nz.org.sesa.los.client.Position
import nz.org.sesa.los.client.Item
import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe.{TypeTag, typeOf}

object Map {
    case class Tile(val pos : Position, val terrain : String, val features : List[String]) {
        override def toString = s"t"
    }

    private var openedMap : Boolean = false

    // cache tiles (we're never going to need to update this once we have them)
    lazy val world : List[Tile] = {
        var http = new Http()
        // load the map tiles on first use of the map
        val req = :/(Global.ServerAddress) / "realms" / "world"
        val js = json.parse(Await.result(http(req), Duration.Inf).getResponseBody())
        http.shutdown()

        implicit val formats = json.DefaultFormats

        val w = (js \ "w").extract[Int]

        val json.JArray(jsRows) = js \ "tiles"

        for { (tile, i) <- jsRows.zipWithIndex } yield {
            val x = i % w
            val y = i / w

            (tile ++ (
                ("pos" ->
                    ("x" -> x) ~
                    ("y" -> y) ~
                    ("realm" -> "world")
                )
            )).extract[Tile]
        }
    }
}

class Map(val id : Int, val owner : Adventurer) extends Item {
    def name = "map"
    def examine = "It's a map, but the legend is missing."
    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream("/images/map.txt")).mkString

    def action[T : TypeTag](args: Any*) = () match {
        case _ if !(typeOf[T] =:= typeOf[List[Map.Tile]]) => {
            Display.show("It seems like this item needs to be used to find a List of Tiles.")
            None
        }

        case _ if args.length != 0 => {
            Display.show("That's ridiculous, you can't use a map like that.")
            None
        }

        case _ => {
            if (!Map.openedMap) {
                Display.show("You open your map, and find that it has a bunch of colored squares. Maybe you can use them with your legend...?")
                Map.openedMap = true;
            }
            Some(Map.world.asInstanceOf[T])
        }
    }
    def ensureRemoting = false
}
