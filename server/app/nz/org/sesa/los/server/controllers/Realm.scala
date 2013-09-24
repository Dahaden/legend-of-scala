package nz.org.sesa.los.server.controllers

import play.api._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

import nz.org.sesa.los.server.models.World

object Realm extends Controller {
    def view(realmName : String) = Action { request =>
        implicit val formats = json.DefaultFormats
        Ok(json.pretty(json.render(decompose(World.world.tiles)))).as("application/json")
    }
}
