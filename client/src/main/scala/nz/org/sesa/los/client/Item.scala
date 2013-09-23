package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._

trait Item {
    val id : Int

    def rejectUse() {
        Display.show(s"You fumble with the $name but can't quite figure out how to use it.")
    }

    def name : String
    def examine : String
    def use[T : Defaults[Any]#To](args: Any*) : T

    override def toString = {
        (this.name.charAt(0) match {
            case 'a' | 'e' | 'i' | 'o' | 'u' => "an"
            case _ => "a"
        }) + " " + this.name
    }
}
