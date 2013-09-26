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

class Weapon(val id : Int, val owner : Adventurer, val material : String, val class_ : String) extends Item {    

    def name = {
        val betterName = material match { 
            case "wood" => "wooden"
            case _      => material
        }
        s"$betterName $class_"
    }

    def examine = s"It's a $class_ made of $material."

    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream(s"/images/${material}_${class_}.txt")).mkString

    def action[T : TypeTag](args: Any*) = None
}
