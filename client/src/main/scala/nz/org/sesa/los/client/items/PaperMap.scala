package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.Global

import nz.org.sesa.los.client.Item

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._

class Tile(val x : Int, val y : Int, val terrain : String, val features : List[String]) {
    override def toString = s"$terrain tile at ($x,$y)"
}

private object PaperMap {
    private val Stride = 150

    // cache tiles (we're never going to need to update this once we have them)
    private var tiles : List[Tile] = null
}

class PaperMap(val id : Int) extends Item {
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
            val req = :/(Global.ServerAddress) / "map"
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
