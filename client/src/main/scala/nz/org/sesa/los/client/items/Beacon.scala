package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.Item

class Beacon(val id : Int) extends Item {
    def name = "handheld beacon"
    def examine = "It's some kind of glowing gem with weird glyphs on it."
    def use(args: Any*) : Any = {
        if (args.length != 0) {
            this.rejectUse()
            return null
        }
    }
}
