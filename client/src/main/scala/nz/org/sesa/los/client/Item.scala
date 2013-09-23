package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._

import scala.language.existentials

trait Item {
    val id : Int

    def rejectUse() {
        Display.show(s"You fumble with the $name but can't quite figure out how to use it.")
    }

    def name : String
    def examine : String

    /**
     * use :: forall a. [Any] -> a
     *
     * Terrible, terrible existential typing.
     */
    def use(args: Any*) : T forSome { type T }

    override def toString = {
        (this.name.charAt(0) match {
            case 'a' | 'e' | 'i' | 'o' | 'u' => "an"
            case _ => "a"
        }) + " " + this.name
    }
}
