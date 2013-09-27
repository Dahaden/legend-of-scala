package nz.org.sesa.los.server.controllers

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._
import java.sql.Connection

import nz.org.sesa.los.server.util
import nz.org.sesa.los.server.models
import nz.org.sesa.los.server.Position

object RealmTileFeature extends Controller {
    private object RemoteOnlyBehaviors {
        def teleport(adventurer : Row, target : Position, changeSpawn : Boolean)(implicit c : Connection) = {
            models.Realm.getRow(target.realm) match {
                case Some(row) => {
                    SQL("""UPDATE adventurers
                           SET x = {x},
                               y = {y},
                               realm_id = {realmId}""" + (
                        if (changeSpawn) """,
                               spawn_x = {x},
                               spawn_y = {y},
                               spawn_realm_id = {realmId}"""
                        else
                            ""
                        ) + """
                           WHERE id = {adventurerId}""").on(
                        "x" -> target.x,
                        "y" -> target.y,
                        "realmId" -> row[Int]("realms.id"),
                        "adventurerId" -> adventurer[Int]("adventurers.id")
                    ).execute()

                    true
                }
                case None => false
            }
        }

        def unpackChest(adventurer : Row, items : json.JArray)(implicit c : Connection) = {
            implicit val formats = json.DefaultFormats

            for {
                item <- items.arr
            } SQL("""INSERT INTO items (kind, owner_id, attrs)
                     VALUES ({kind}, {ownerId}, {attrs})""").on(
                "kind" -> (item \ "kind").extract[String],
                "ownerId" -> adventurer[Int]("adventurers.id"),
                "attrs" -> json.pretty(json.render(item \ "attrs"))
            ).execute()
        }
    }

    private def getRow(realmName : String, x : Int, y : Int, featureId : Int)(implicit c : Connection) = {
        val rows = SQL("""SELECT *
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

    def view(realmName : String, x : Int, y : Int, featureId : Int) = Action { request =>
        DB.withTransaction { implicit c =>
            this.getRow(realmName, x, y, featureId) match {
                case None => {
                    NotFound(json.pretty(json.render(
                        ("why" -> s"Er, that doesn't exist anymore.")
                    ))).as("application/json")
                }
                case Some(row) => {
                    Ok(json.pretty(json.render(
                        ("id" -> row[Int]("features.id")) ~
                        ("kind" -> row[String]("features.kind")) ~
                        ("attrs" -> json.parse(row[String]("features.attrs")))
                    ))).as("application/json")
                }
            }
        }
    }

    def use(realmName : String, x : Int, y : Int, featureId : Int) = Action(parse.tolerantText) { request =>
        DB.withTransaction { implicit c =>
            val adventurerOption = for {
                (username, password) <- util.getBasicAuth(request)
                row <- models.Adventurer.getAuthRow(username, password)
            } yield row

            (for {
                _ <- adventurerOption
                row <- this.getRow(realmName, x, y, featureId)
            } yield row) match {
                case None => {
                    NotFound(json.pretty(json.render(
                        ("why" -> s"Er, that doesn't exist anymore.")
                    ))).as("application/json")
                }
                case Some(row) => {
                    val kind = row[String]("features.kind")
                    val attrs = json.parse(row[String]("features.attrs"))

                    // XXX: make this less arrow code-y
                    kind match {
                        case "remote_only" => {
                            implicit val formats = json.DefaultFormats

                            val behavior = (attrs \ "behavior").extract[String]
                            behavior match {
                                case "portal" => {
                                    adventurerOption match {
                                        case Some(adventurer) => {
                                            if (RemoteOnlyBehaviors.teleport(adventurer, (attrs \ "target").extract[Position],
                                                                             (attrs \ "change_spawn").extract[Option[Boolean]].getOrElse(false))) {
                                                // stop adventurers from visiting the same portal twice
                                                (attrs \ "linked_portal_id").extract[Option[Int]] match {
                                                    case None => { }
                                                    case Some(portalId) => {
                                                        SQL("""DELETE FROM features
                                                               WHERE id = {portalId}""").on(
                                                            "portalId" -> portalId
                                                        ).execute
                                                    }
                                                }

                                                Ok(json.pretty(json.render(
                                                    ("why" -> s"Whoosh! Your surroundings disappear as you hurtle through the portal into a new realm.")
                                                ))).as("application/json")
                                            } else {
                                                BadRequest(json.pretty(json.render(
                                                    ("why" -> s"Teleport failed?")
                                                ))).as("application/json")
                                            }
                                        }
                                        case None => BadRequest(json.pretty(json.render(
                                            ("why" -> s"Need adventurer.")
                                        ))).as("application/json")
                                    }
                                }

                                case "chest" => {
                                    adventurerOption match {
                                        case Some(adventurer) => {
                                            SQL("""DELETE FROM features
                                                   WHERE id = {featureId}""").on(
                                                "featureId" -> featureId
                                            ).execute

                                            RemoteOnlyBehaviors.unpackChest(adventurer, (attrs \ "items").extract[json.JArray])

                                            // XXX: yuck, we have to do this
                                            Ok(json.pretty(json.render(
                                                ("why" -> s"You open the chest and find some items.")
                                            ))).as("application/json")
                                        }
                                        case None => BadRequest(json.pretty(json.render(
                                            ("why" -> s"Need adventurer.")
                                        ))).as("application/json")
                                    }
                                }
                            }
                        }
                        case _ => BadRequest(json.pretty(json.render(
                            ("why" -> s"Can't use this feature.")
                        ))).as("application/json")
                    }
                }
            }
        }
    }
}
