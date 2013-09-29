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
            case "plank" => "It's a wooden plank."
            case "ingot" => "It's an ingot made out of iron."
            case "diamond" => "It's a glistening diamond."
            case "fire gem" => "It's a fire gem, one of the parts of an ancient staff."
            case "earth gem" => "It's a earth gem, one of the parts of an ancient staff."
            case "water gem" => "It's a water gem, one of the parts of an ancient staff."
            case "air gem" => "It's a air gem, one of the parts of an ancient staff."
        }
    }

    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream(s"/images/${name.replace(" ", "_")}.txt")).mkString

    def action[T : TypeTag](args: Any*) = {
        Display.show("Have you tried .combineing this with something else?")
        None
    }
}
