package nz.org.sesa.los.client.features

import nz.org.sesa.los.client.Adventurer
import nz.org.sesa.los.client.Global
import nz.org.sesa.los.client.Position
import nz.org.sesa.los.client.Feature
import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe.{TypeTag, typeOf}

class RemoteOnlyFeature(val id : Int, val kind : String, val bindee : Adventurer) extends Feature {
    def name = kind

    def examine = kind match {
        case "portal" => "It's a portal to a different realm."
        case "chest" => "It's a sturdy wooden chest. You can open it."
    }
    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream(kind match {
        case "portal" => "/images/portal.txt"
        case "chest" => "/images/chest.txt"
    })).mkString

    def action[T : TypeTag](args: Any*) = () match {
        case _ if args.length > 0 => {
            Display.show(kind match {
                case "portal" => "Yeah, you're just going to have to go in there yourself."
                case "chest" => "Seriously, what are you trying to use this chest with?"
            })
            None
        }

        case _ => {
            val req = :/(Global.ServerAddress) / "realms" / bindee.pos.realm / (bindee.pos.x.toString + "," + bindee.pos.y.toString) / "features" / this.id <<? Map("adventurerName" -> bindee.name) << ""

            implicit val formats = json.DefaultFormats

            val resp = Await.result(Global.http(req), Duration.Inf)
            var js = json.parse(resp.getResponseBody())

            Display.show((js \ "why").extract[String])
            None
        }
    }
}
