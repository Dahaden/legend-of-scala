package nz.org.sesa.los.server.controllers

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

object RealmTileFeature extends Controller {
    private def getRow(realmName : String, x : Int, y : Int, featureId : Int) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT features.id AS id,
                                     features.kind AS kind,
                                     features.attrs AS attrs,
                                     features.x AS x,
                                     features.y as y,
                                     realms.name AS realm
                              FROM features, realms
                              WHERE features.realm_id = realms.id AND
                                    realms.name = {name} AND
                                    features.x = {x} AND
                                    features.y = {y} AND
                                    features.id = {featureId}""").on(
                "name" -> realmName,
                "x" -> x,
                "y" -> y,
                "featureId" -> featureId
            )

            rows().toList match {
                case Nil => None
                case row::_ => Some(row)
            }
        }
    }

    def view(realmName : String, x : Int, y : Int, featureId : Int) = Action { request =>
        this.getRow(realmName, x, y, featureId) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Er, that doesn't exist anymore.")
                ))).as("application/json")
            }
            case Some(row) => {
                Ok(json.pretty(json.render(
                    ("id" -> row[Int]("id")) ~
                    ("kind" -> row[String]("kind")) ~
                    ("attrs" -> json.parse(row[Option[String]]("attrs").getOrElse("null")))
                ))).as("application/json")
            }
        }
    }

    def use(realmName : String, x : Int, y : Int, featureId : Int, adventurerName : Option[String]) = Action { request =>
        BadRequest(json.pretty(json.render(
            ("why" -> s"Can't use this feature.")
        ))).as("application/json")
    }
}
