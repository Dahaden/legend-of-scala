package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._
import net.liftweb.json

trait Feature {
    val id : Int
    val attrs : json.JObject

    def name : String
    def image : String
    def examine : String

    def use[T : Manifest] : Option[T] = this.use()

    def use[T : Manifest](args: Any*) : Option[T] = {
        this.action(args: _*)
    }

    protected def action[T : Manifest](args: Any*) : Option[T]

    override def toString = {
        val imageLines = this.image.split("\n")

        "\n" + imageLines.zipWithIndex.map { case (l, i) =>
            if (i == imageLines.length / 2) {
                val numSpaces = l.filter({x => x == ' '}).length
                l + (numSpaces to 28).map(_ => " ").mkString + this.name
            } else {
                l
            }
        }.mkString("\n") + "\n"
    }
}
