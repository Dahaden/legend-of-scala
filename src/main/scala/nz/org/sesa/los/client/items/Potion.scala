package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.Adventurer
import nz.org.sesa.los.client.Global
import nz.org.sesa.los.client.Position
import nz.org.sesa.los.client.Item
import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe.{TypeTag, typeOf}

class Potion(val id : Int, val owner : Adventurer) extends Item {
    def name = "potion"

    def examine = "It's a health potion. It seems a little questionable to be drinking strange bottles of liquids you find but, you know, #YOLO"

    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream("/images/potion.txt")).mkString

    def action[T : TypeTag](args: Any*) = () match {
        case _ if args.length > 0 => {
            Display.show("Nuh-uh, you can't use that with something else.")
            None
        }

        case _ => {
            val req = (:/(Global.ServerAddress) / "adventurers" / owner.name /
                "items" / this.id / "use" << "").as_!(owner.name, owner.token)

            implicit val formats = json.DefaultFormats

            val resp = Await.result(owner.http(req), Duration.Inf)
            var js = json.parse(resp.getResponseBody())

            Display.show((js \ "why").extract[String])
            this.owner.refresh
            None
        }
    }
}
