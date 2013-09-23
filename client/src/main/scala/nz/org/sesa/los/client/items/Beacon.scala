package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.util._
import nz.org.sesa.los.client.Item

object Beacon {
    case class Signal(val x : Int, val y : Int, val name : String, val kind : String) {
        override def toString = s"$kind $name at ($x, $y)"
    }

    object Signal {
        val Player : String = "player"
    }
}

class Beacon(val id : Int, val owner : String) extends Item {
    def name = "handheld beacon"
    def examine = "It's some kind of glowing gem with weird glyphs on it. You can use it to find things on the map."
    def action[T : Manifest](args: Any*) = {
        val m = manifest[T]

        if (m == manifest[List[Beacon.Signal]]) {
            List[Beacon.Signal]()
        } else {
            Display.show("It looks like you can use the beacon to find a List of Signals.")
            throw new Item.OAK()
        }
    }.asInstanceOf[T]
    def ensureRemoting = false
}
