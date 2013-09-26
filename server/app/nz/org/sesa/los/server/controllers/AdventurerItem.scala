package nz.org.sesa.los.server.controllers

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import nz.org.sesa.los.server.models

object AdventurerItem extends Controller {
    def index(adventurerName : String) = Action { request =>
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT *
                              FROM items, adventurers
                              WHERE items.owner_id = adventurers.id AND
                              adventurers.name = {owner}""").on(
                "owner" -> adventurerName
            )

            Ok(json.pretty(json.render(rows().map { row =>
                ("id" -> row[Int]("items.id")) ~
                ("kind" -> row[String]("items.kind")) ~
                ("attrs" -> json.parse(row[Option[String]]("items.attrs").getOrElse("{}"))) ~
                ("owner" -> row[String]("adventurers.name"))
            }))).as("application/json")
        }
    }

    def view(adventurerName : String, itemId : Int) = Action { request =>
        models.Adventurer.getItem(itemId, adventurerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"You don't have this item anymore.")
                ))).as("application/json")
            }
            case Some(row) => {
                Ok(json.pretty(json.render(
                    ("id" -> row[Int]("items.id")) ~
                    ("kind" -> row[String]("items.kind")) ~
                    ("attrs" -> json.parse(row[Option[String]]("items.attrs").getOrElse("{}"))) ~
                    ("owner" -> row[String]("adventurers.name"))
                ))).as("application/json")
            }
        }
    }

    def use(adventurerName : String, itemId : Int) = Action(parse.tolerantText) { request =>
        BadRequest(json.pretty(json.render(
            ("why" -> s"Can't use this item.")
        ))).as("application/json")
    }

    def combine(adventurerName : String) = Action(parse.tolerantText) { request =>
        implicit val formats = json.DefaultFormats

        val parts = json.parse(request.body).extract[Map[String, Int]]

        // ensure all the parts exist
        val items = (for {
            (slot, itemId) <- parts
            row <- models.Adventurer.getItem(itemId, adventurerName)
        } yield (slot -> row)).toMap

        val slots = parts.keySet

        if (slots != items.keySet) {
            BadRequest(json.pretty(json.render(
                ("why" -> s"You try to combine the items, but find that some of them don't exist anymore.")
            ))).as("application/json")
        } else if (items.values.toSet.size != slots.size) {
            BadRequest(json.pretty(json.render(
                ("why" -> s"You try to combine an item... with itself?")
            ))).as("application/json")
        } else {
            (() match {
                case _ if slots == Set("head", "handle") => {
                    // mace
                    val handle = items.get("handle").head
                    val head = items.get("head").head

                    if (head[String]("items.kind") != "part" || handle[String]("items.kind") != "part" || (json.parse(handle[String]("items.attrs")) \ "type").extract[String] != "stick") {
                        None
                    } else {
                        for {
                            material <- (json.parse(head[String]("items.attrs")) \ "type").extract[String] match {
                                case "plank" => Some("wood")
                                case "ingot" => Some("iron")
                                case "diamond" => Some("diamond")
                                case _ => None
                            }
                        } yield ("weapon", (
                            ("class" -> "mace") ~
                            ("material" -> material))
                        )
                    }
                }
                case _ if slots == Set("pole", "tip") => {
                    // spear
                    val pole = items.get("pole").head
                    val tip = items.get("tip").head

                    if (tip[String]("items.kind") != "part" || pole[String]("items.kind") != "part" || (json.parse(pole[String]("items.attrs")) \ "type").extract[String] != "stick") {
                        None
                    } else {
                        for {
                            material <- (json.parse(tip[String]("items.attrs")) \ "type").extract[String] match {
                                case "plank" => Some("wood")
                                case "ingot" => Some("iron")
                                case "diamond" => Some("diamond")
                                case _ => None
                            }
                        } yield ("weapon", (
                            ("class" -> "spear") ~
                            ("material" -> material))
                        )
                    }
                }
                case _ if slots == Set("hilt", "blade") => {
                    // sword
                    val hilt = items.get("hilt").head
                    val blade = items.get("blade").head

                    if (blade[String]("items.kind") != "part" || hilt[String]("items.kind") != "part" || (json.parse(hilt[String]("items.attrs")) \ "type").extract[String] != "stick") {
                        None
                    } else {
                        for {
                            material <- (json.parse(blade[String]("items.attrs")) \ "type").extract[String] match {
                                case "plank" => Some("wood")
                                case "ingot" => Some("iron")
                                case "diamond" => Some("diamond")
                                case _ => None
                            }
                        } yield ("weapon", (
                            ("class" -> "sword") ~
                            ("material" -> material))
                        )
                    }
                }
                case _ => {
                    None
                }
            }) match {
                case None => BadRequest(json.pretty(json.render(
                    ("why" -> s"You try to combine the items, but don't quite manage.")
                ))).as("application/json")

                case Some((kind, attrs)) => {
                    var ownerId = items.values.head[Int]("items.owner_id")

                    val itemId = DB.withTransaction { implicit c =>
                        for {
                            part <- items.values
                        } SQL("""DELETE FROM items
                                 WHERE id = {id}""").on(
                            "id" -> part[Int]("items.id")
                        ).execute()

                        SQL("""INSERT INTO items (kind, owner_id, attrs)
                               VALUES ({kind}, {ownerId}, {attrs})""").on(
                            "kind" -> kind,
                            "ownerId" -> ownerId,
                            "attrs" -> json.pretty(json.render(attrs))
                        ).executeInsert().get
                    }

                    Ok(json.pretty(json.render(
                        "item_id" -> itemId
                    ))).as("application/json")
                }
            }
        }
    }

    def separate(adventurerName : String, itemId : Int) = Action { request =>
        models.Adventurer.getItem(itemId, adventurerName) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"You don't have this item anymore.")
                ))).as("application/json")
            }
            case Some(row) => {
                val attrs = json.parse(row[String]("items.attrs"))
                implicit val formats = json.DefaultFormats

                row[String]("items.kind") match {
                    case "weapon" => {
                        val rest = (attrs \ "material").extract[String] match {
                            case "wood" => "plank"
                            case "iron" => "ingot"
                            case "diamond" => "diamond"
                        }

                        val itemIds = DB.withTransaction { implicit c =>
                            val stickId = SQL("""INSERT INTO items (kind, owner_id, attrs)
                                                 VALUES ('part', {ownerId}, {attrs})""").on(
                                "ownerId" -> row[Int]("items.owner_id"),
                                "attrs" -> json.pretty(json.render(
                                    "type" -> "stick"
                                ))
                            ).executeInsert().get

                            val restId = SQL("""INSERT INTO items (kind, owner_id, attrs)
                                                VALUES ('part', {ownerId}, {attrs})""").on(
                                "ownerId" -> row[Int]("items.owner_id"),
                                "attrs" -> json.pretty(json.render(
                                    "type" -> rest
                                ))
                            ).executeInsert().get

                            SQL("""DELETE FROM items
                                   WHERE id = {id}""").on(
                                "id" -> row[Int]("items.id")
                            ).execute()

                           List(stickId, restId)
                        }

                        Ok(json.pretty(json.render(
                            "item_ids" -> itemIds
                        ))).as("application/json")
                    }
                    case _ => BadRequest(json.pretty(json.render(
                        // TODO: implement separations, probably from combines
                        ("why" -> s"Can't separate this item.")
                    ))).as("application/json")
                }
            }
        }
    }
}
