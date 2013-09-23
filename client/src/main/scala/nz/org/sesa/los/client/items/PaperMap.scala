package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.Global
import nz.org.sesa.los.client.Item
import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._

object PaperMap {
    private val Stride = 150

    // cache tiles (we're never going to need to update this once we have them)
    private var tiles : List[Tile] = null

    case class Tile(val x : Int, val y : Int, val terrain : String, val features : List[String]) {
        override def toString = s"$terrain at ($x, $y)"
    }
}

class PaperMap(val id : Int, val owner : String) extends Item {
    def name = "paper map"
    def examine = "It's a map, but the legend is missing."
    def action[T : Manifest](args: Any*) = {
        val m = manifest[T]

        if (args.length != 0) {
            Display.show("That's ridiculous, you can't use a map like that.")
            throw new Item.OAK()
        } else {
            if (m == manifest[List[PaperMap.Tile]]) {
                if (PaperMap.tiles == null) {
                    Display.show("You open your map, and find that it has a bunch of colored squares. Maybe you can use them with your legend...?")

                    // load the map tiles on first use of the map
                    val req = :/(Global.ServerAddress) / "map"
                    val json.JArray(js) = json.parse(Await.result(Global.http(req), Duration.Inf).getResponseBody())

                    implicit val formats = json.DefaultFormats

                    PaperMap.tiles = for { (tile, i) <- js.zipWithIndex } yield {
                        val x = i % PaperMap.Stride
                        val y = i / PaperMap.Stride

                        (tile ++ (("x" -> x) ~ ("y" -> y))).extract[PaperMap.Tile]
                    }
                }
                PaperMap.tiles
            } else {
                Display.show("It seems like this item needs to be used as a List of Tiles.")
                throw new Item.OAK()
            }
        }
    }.asInstanceOf[T]
    def ensureRemoting = false
}
