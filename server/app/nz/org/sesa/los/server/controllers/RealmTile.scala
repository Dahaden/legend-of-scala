package nz.org.sesa.los.server.controllers

import play.api._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

import nz.org.sesa.los.server.models.World

object RealmTile extends Controller {
    def view(realmName : String, x : Int, y : Int) = Action { request =>
        val i = y * World.Stride + x
        val tile = World.world.tiles(i)

        Ok(json.pretty(json.render(
            ("terrain" -> tile.terrain) ~
            ("pos" ->
                ("x" -> x) ~
                ("y" -> y) ~
                ("realm" -> realmName)
            )
        ))).as("application/json")
    }
}
