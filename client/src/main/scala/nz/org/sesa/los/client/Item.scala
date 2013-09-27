package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.Serialization._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe.{TypeTag, typeTag}

object Item {
    /**
     * Reference to a remote object. The adventurer should never be concerned with
     * this class directly, as it will deserialize into an Item.
     */
    case class RemoteHandle(id : Int, kind : String, owner : String, attrs : json.JObject) {
        def deserialize(owner : Adventurer) = {
            implicit val formats = json.DefaultFormats

            kind match {
                case "map"          => new items.Map(id, owner)
                case "map-legend"   => new items.MapLegend(id, owner)
                case "beacon"       => new items.Beacon(id, owner)
                case "weapon"       => new items.Weapon(id, owner, (attrs \ "material").extract[String], (attrs \ "class").extract[String])
                case "part"         => new items.Part(id, owner, (attrs \ "type").extract[String])
                case "potion"       => new items.Potion(id, owner)
            }
        }
    }
}

trait Item {
    val id : Int
    val owner : Adventurer

    def name : String
    def image : String
    def examine : String

    def remoting : Boolean = true

    def use[T : TypeTag] : Option[T] = this.use()
    def use[T : TypeTag](args: Any*) : Option[T] = {
        if (!this.remoting) {
            return this.action(args: _*)
        }

        // ensure the item is still remoting for the owner
        val req = :/(Global.ServerAddress) / "adventurers" / owner.name / "items" / this.id

        implicit val formats = json.DefaultFormats

        val resp = Await.result(this.owner.http(req), Duration.Inf)
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

    protected def action[T : TypeTag](args: Any*) : Option[T]

    def separate() : List[Item] = {
        implicit val formats = json.DefaultFormats

        val req = (:/(Global.ServerAddress) / "adventurers" / owner.name / "items" / this.id.toString / "separate" << "").as_!(owner.name, owner.token)
        val resp = Await.result(owner.http(req), Duration.Inf)

        val js = json.parse(resp.getResponseBody())

        resp.getStatusCode() match {
            case 400 => {
                Display.show(s"You try to pull the ${this.name} apart, but it doesn't budge.")
                List()
            }
            case 200 => {
                Display.show(s"You pull apart the ${this.name}.")
                owner.inventory.filter {(js \ "item_ids").extract[List[Int]] contains _.id}
            }
        }
    }

    override def toString = s"""
${this.image}
${Display.StartHilight}.name =${Display.Reset} ${this.name}
"""
}
