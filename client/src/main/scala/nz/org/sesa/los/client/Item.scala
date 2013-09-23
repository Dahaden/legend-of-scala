package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json

import scala.concurrent._
import scala.concurrent.duration._

object Item {
    class OAK extends RuntimeException("There's a time and place for everything, but not now.")
}

trait Item {
    val id : Int
    val owner : String

    def name : String
    def examine : String

    def use[T : Manifest](args: Any*) : T = {
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
                throw new Item.OAK()
            }
        }
    }

    def action[T : Manifest](args: Any*) : T

    override def toString = {
        (this.name.charAt(0) match {
            case 'a' | 'e' | 'i' | 'o' | 'u' => "an"
            case _ => "a"
        }) + " " + this.name
    }
}
