package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._
import net.liftweb.json
import scala.reflect.runtime.universe.TypeTag

object Feature {
    case class RemoteHandle(id : Int, kind : String, attrs : json.JObject) {
        def deserialize = {
            kind match {
                case "chest"        => new features.Chest(id)
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

    override def toString = {
        val imageLines = this.image.split("\n")

        "\n" + imageLines.zipWithIndex.map { case (l, i) =>
            if (i == imageLines.length / 2) {
                val numSpaces = l.filter({x => x == ' '}).length
                l + (numSpaces to 36).map(_ => " ").mkString + this.name
            } else {
                l
            }
        }.mkString("\n") + "\n"
    }
}
