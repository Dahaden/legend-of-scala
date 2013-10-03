package nz.org.sesa.los.client.items

import nz.org.sesa.los.client.Adventurer
import nz.org.sesa.los.client.Global
import nz.org.sesa.los.client.Position
import nz.org.sesa.los.client.Item
import nz.org.sesa.los.client.Monster
import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe.{TypeTag, typeOf}

class Weapon(val id : Int, val owner : Adventurer, val material : String, val class_ : String) extends Item {
    def name = class_ match {
        case "ancient_staff" => "ancient staff"
        case _ => {
            val betterName = material match {
                case "wood" => "wooden"
                case _      => material
            }
            s"$betterName $class_"
        }
    }

    def examine = class_ match {
        case "ancient_staff" => "It's an ancient staff with ancient powers. It kind of looks like a fishing rod."
        case _ => s"It's a $class_ made of $material."
    }

    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream(s"/images/${material}_${class_}.txt")).mkString

    def action[T : TypeTag](args: Any*) = () match {
        case _ if args.length != 1 => {
            Display.show("You should use this on, like, a monster.")
            None
        }

        case _ if !args(0).isInstanceOf[Monster] => {
            Display.show("I don't think that appreciates being attacked.")
            None
        }

        case _ => {
            val monsterId = args(0).asInstanceOf[Monster].id
            val pos = owner.look.pos

            val req = (:/(Global.ServerAddress) / "realms" / pos.realm / (pos.x.toString + "," + pos.y.toString) / "monsters" / monsterId / "attack" << json.pretty(json.render(
                "weapon_id" -> this.id
            ))).as_!(owner.name, owner.token)

            implicit val formats = json.DefaultFormats

            val resp = Await.result(owner.http(req), Duration.Inf)
            var js = json.parse(resp.getResponseBody())

            Display.show((js \ "why").extract[String])
            this.owner.refresh
            None
        }
    }
}
