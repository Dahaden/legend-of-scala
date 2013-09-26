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

class Part(val id : Int, val owner : Adventurer, type_ : String) extends Item {
    def name = type_

    def examine = {
        type_ match {
            case "stick" => "It's a wooden stick. You could probably fashion it into some kind of handle."
            case "plank" => "It's a wooden plank. It would make an okay sword blade or ax head."
            case "ingot" => "It's an ingot made out of steel. Maybe you could turn it into a sword blade or ax head?"
            case "diamond" => "It's a glistening diamond. It would make a really good sword blade or ax head."
        }
    }

    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream(s"/images/${name}.txt")).mkString

    def action[T : TypeTag](args: Any*) = {
        None
    }
}
