package nz.org.sesa.los.client.features

import nz.org.sesa.los.client.Feature
import nz.org.sesa.los.client.Images
import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe.{TypeTag, typeOf}

class Chest(val id : Int) extends Feature {
    def name = "chest"
    def examine = "It's a sturdy wooden chest. You can open it."
    def image = Images.Beacon

    def action[T : TypeTag](args: Any*) = () match {
        case _ if args.length > 0 => {
            Display.show("Um, just try opening it?")
            None
        }

        case _ => {
            None
        }
    }
}
