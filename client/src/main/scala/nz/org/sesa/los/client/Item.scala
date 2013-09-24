package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.Serialization._

import scala.concurrent._
import scala.concurrent.duration._

trait Item {
    val id : Int
    val attrs : json.JObject
    val owner : Adventurer

    def name : String
    def image : String
    def examine : String

    def remoting : Boolean = true

    def use[T : Manifest] : Option[T] = this.use()

    def use[T : Manifest](args: Any*) : Option[T] = {
        if (!this.remoting) {
            return this.action(args: _*)
        }

        // ensure the item is still remoting for the owner
        val req = :/(Global.ServerAddress) / "adventurers" / owner.name / "items" / this.id

        implicit val formats = json.DefaultFormats

        val resp = Await.result(Global.http(req), Duration.Inf)
        var js = json.parse(resp.getResponseBody())

        resp.getStatusCode() match {
            case 200 => {
                this.action(args: _*)
            }

            case 404 => {
                Display.show((js \ "why").extract[String])
                None
            }
        }
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
