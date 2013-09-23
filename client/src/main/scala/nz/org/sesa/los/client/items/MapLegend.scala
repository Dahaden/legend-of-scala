package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.Item
import nz.org.sesa.los.client.util._

private object MapLegend {
    val Colors = scala.collection.immutable.Map(
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

class MapLegend(val id: Int, val owner : String) extends Item {
    def name = "map legend"
    def examine = "It's a torn off piece of paper, with some kind of map legend on it."
    override def remoting : Boolean = false
    def action[T : Manifest](args: Any*) = {
        val m = manifest[T]

        if (m == manifest[String]) {
            if (args.length != 1) {
                Display.show("Hm, you need something to use this legend with... maybe a map tile?")
                throw new Item.OAK()
            }

            val s = args(0).asInstanceOf[Map.Tile].terrain
            val c = MapLegend.Colors.getOrElse(s, 0)

            Display.fg(c) + Display.bg(c) + "  " + Display.Reset
        } else {
            Display.show("It looks like you can use this legend to find Strings.")
            throw new Item.OAK()
        }
    }.asInstanceOf[T]
}
