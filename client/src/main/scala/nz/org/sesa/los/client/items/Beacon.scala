package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.util._
import nz.org.sesa.los.client.Item

object Beacon {
    type Coordinate = (Int, Int)
}

class Beacon(val id : Int) extends Item {
    def name = "handheld beacon"
    def examine = "It's some kind of glowing gem with weird glyphs on it. You can use it to find things on the map."
    def use(args: Any*) : Any = {
        if (args.length == 0) {
            println(s"""${Display.StartHilight}A voice mysteriously emanates from the beacon.${Display.Reset}
Speak into the beacon your query -- whether "dungeons", "players" or "mines" -- and it shall be found.
""")
            Set[Beacon.Coordinate]()
        } else {
            Set[Beacon.Coordinate]()
        }
    }
}
