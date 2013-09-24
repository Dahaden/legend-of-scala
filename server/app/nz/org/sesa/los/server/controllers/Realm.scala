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
    private def getRow(realmName : String) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT id, name, w, h
                              FROM realms
                              WHERE name = {name}""").on(
                "name" -> realmName
            )

            rows().toList match {
                case Nil => None
                case row::_ => Some(row)
            }
        }
    }

    def view(realmName : String) = Action { request =>
        implicit val formats = json.DefaultFormats

        this.getRow(realmName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"No such realm.")
                ))).as("application/json")
            }
            case Some(row) => {
                Ok(json.pretty(json.render(
                    ("id" -> row[Int]("id")) ~
                    ("name" -> row[String]("name")) ~
                    ("w" -> row[Int]("w")) ~
                    ("h" -> row[Int]("h")) ~
                    ("tiles" -> decompose(models.Realm.loadTiles(realmName)))
                ))).as("application/json")
            }
        }
    }
}
