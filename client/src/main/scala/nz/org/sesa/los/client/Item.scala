package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json

import scala.concurrent._
import scala.concurrent.duration._

trait Item {
    val id : Int
    val owner : String

    def name : String
    def image : String
    def examine : String

    def remoting : Boolean = true

    def use[T : Manifest](args: Any*) : Option[T] = {
        if (!this.remoting) {
            return this.action(args: _*)
        }

        // ensure the item is still remoting for the owner
        val req = :/(Global.ServerAddress) / "adventurers" / owner / "items" / this.id

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

    def action[T : Manifest](args: Any*) : Option[T]

    override def toString = {
        "\n" + Display.StartHilight +
        (this.name.charAt(0) match {
            case 'a' | 'e' | 'i' | 'o' | 'u' => "an"
            case _ => "a"
        }) + " " + this.name + Display.Reset + "\n\n" +
        this.image + "\n"
    }
}
