package nz.org.sesa.los.server.controllers

import anorm._
import play.api._
import play.api.Play.current
import play.api.mvc._
import play.api.db._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import net.liftweb.json.Extraction._

import nz.org.sesa.los.server.models

object Realm extends Controller {
    def view(realmName : String) = Action { request =>
        implicit val formats = json.DefaultFormats

        DB.withTransaction { implicit c =>
            models.Realm.getRow(realmName) match {
                case None => {
                    NotFound(json.pretty(json.render(
                        ("why" -> s"No such realm.")
                    ))).as("application/json")
                }
                case Some(row) => {
                    Ok(json.pretty(json.render(
                        ("id" -> row[Int]("realms.id")) ~
                        ("name" -> row[String]("realms.name")) ~
                        ("w" -> row[Int]("realms.w")) ~
                        ("h" -> row[Int]("realms.h")) ~
                        ("tiles" -> decompose(models.Realm.loadTiles(realmName)))
                    ))).as("application/json")
                }
            }
        }
    }
}
