package nz.org.sesa.los.client.features

import nz.org.sesa.los.client.Position
import nz.org.sesa.los.client.Feature
import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe.{TypeTag, typeOf}

class Portal(val id : Int) extends Feature {
    def name = "portal"
    def examine = "It's a portal to a different realm."
    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream("/images/portal.txt")).mkString

    def action[T : TypeTag](args: Any*) = () match {
        case _ if args.length > 0 => {
            Display.show("Yeah, you're going to have to go through there yourself.")
            None
        }

        case _ => {
            None
        }
    }
}
