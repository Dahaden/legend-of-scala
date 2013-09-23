package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.Item
import nz.org.sesa.los.client.util._

private object Legend {
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

class Legend(val id: Int) extends Item {
    def name = "map legend"
    def examine = "It's a torn off piece of paper, with some kind of map legend on it."
    def use[T : Defaults[Any]#To](args: Any*) = {
        if (args.length != 1) {
            this.rejectUse()
            "  "
        } else {
            val s = args(0).asInstanceOf[Tile].terrain
            val c = Legend.Colors.getOrElse(s, 0)

            Display.fg(c) + Display.bg(c) + "  " + Display.Reset
        }
    }.asInstanceOf[T]
}
