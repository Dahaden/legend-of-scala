package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._
import net.liftweb.json
import scala.reflect.runtime.universe.TypeTag

object Feature {
    case class RemoteHandle(id : Int, kind : String, attrs : json.JObject) {
        def deserialize(bindee : Adventurer) = {
            implicit val formats = json.DefaultFormats

            kind match {
                case "remote_only"  => new features.RemoteOnlyFeature(id, (attrs \ "behavior").extract[String], bindee)
            }
        }
    }
}

trait Feature {
    val id : Int

    def name : String
    def image : String
    def examine : String

    def use[T : TypeTag] : Option[T] = this.use()
    def use[T : TypeTag](args: Any*) : Option[T] = {
        this.action(args: _*)
    }

    protected def action[T : TypeTag](args: Any*) : Option[T]

    override def toString = s"""
${this.image}
${Display.StartHilight}.name =${Display.Reset} ${this.name}
"""
}
