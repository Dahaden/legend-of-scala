package nz.org.sesa.los.server.controllers

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import play.api.mvc._
import net.liftweb.json
import net.liftweb.json.JsonDSL._

import nz.org.sesa.los.server.models
import nz.org.sesa.los.server.util

object AdventurerItem extends Controller {
    def index(adventurerName : String) = Action { request =>
        DB.withTransaction { implicit c =>
            val rows = SQL("""SELECT *
                              FROM items, adventurers
                              WHERE items.owner_id = adventurers.id AND
                              adventurers.name = {owner}""").on(
                "owner" -> adventurerName
            )

            Ok(json.pretty(json.render(rows().map { row =>
                ("id" -> row[Int]("items.id")) ~
                ("kind" -> row[String]("items.kind")) ~
                ("attrs" -> json.parse(row[String]("items.attrs"))) ~
                ("owner" -> row[String]("adventurers.name"))
            }))).as("application/json")
        }
    }

    def view(adventurerName : String, itemId : Int) = Action { request =>
        DB.withTransaction { implicit c =>
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
                        ("attrs" -> json.parse(row[String]("items.attrs"))) ~
                        ("owner" -> row[String]("adventurers.name"))
                    ))).as("application/json")
                }
            }
        }
    }

    def combine(adventurerName : String) = Action(parse.tolerantText) { request =>
        DB.withTransaction { implicit c =>
            val adventurerOption = for {
                (username, password) <- util.getBasicAuth(request)
                row <- models.Adventurer.getAuthRow(username, password)
            } yield row

            implicit val formats = json.DefaultFormats

            val parts = json.parse(request.body).extract[Map[String, Int]]

            // ensure all the parts exist
            val items = adventurerOption.fold { Map() : Map[String, SqlRow] } { _ => (for {
                (slot, itemId) <- parts
                row <- models.Adventurer.getItem(itemId, adventurerName)
            } yield (slot -> row)).toMap }

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
                    case _ if slots == Set("shaft", "fireGem", "waterGem", "earthGem", "airGem") => {
                        // ancient staff
                        val shaft = items.get("shaft").head // tee hee
                        val fireGem = items.get("fireGem").head
                        val waterGem = items.get("waterGem").head
                        val earthGem = items.get("earthGem").head
                        val airGem = items.get("airGem").head

                        if (shaft[String]("items.kind") != "part" || (json.parse(shaft[String]("items.attrs")) \ "type").extract[String] != "stick") {
                            None
                        } else {
                            if (List(fireGem, waterGem, earthGem, airGem).zip(List("fire gem", "water gem", "earth gem", "air gem")).forall({ case (item, type_) =>
                                item[String]("items.kind") == "part" && (json.parse(item[String]("items.attrs")) \ "type").extract[String] == type_
                            })) {
                                Some(("weapon", (
                                    ("class" -> "ancient_staff") ~
                                    ("material" -> "immaterial")
                                )))
                            } else None
                        }
                    }

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

                        for {
                            part <- items.values
                        } SQL("""DELETE FROM items
                                 WHERE id = {id}""").on(
                            "id" -> part[Int]("items.id")
                        ).execute()

                        var itemId = SQL("""INSERT INTO items (kind, owner_id, attrs)
                                            VALUES ({kind}, {ownerId}, {attrs})""").on(
                            "kind" -> kind,
                            "ownerId" -> ownerId,
                            "attrs" -> json.pretty(json.render(attrs))
                        ).executeInsert().get

                        Ok(json.pretty(json.render(
                            "item_id" -> itemId
                        ))).as("application/json")
                    }
                }
            }
        }
    }

    def use(adventurerName : String, itemId : Int) = Action { request =>
        DB.withTransaction { implicit c =>
            val adventurerOption = for {
                (username, password) <- util.getBasicAuth(request)
                row <- models.Adventurer.getAuthRow(username, password)
            } yield row

            (for {
                adventurer <- adventurerOption
                row <- models.Adventurer.getItem(itemId, adventurerName)
            } yield (adventurer, row)) match {
                case None => {
                    NotFound(json.pretty(json.render(
                        ("why" -> s"You don't have this item anymore.")
                    ))).as("application/json")
                }
                case Some((adventurer, item)) => {
                    if (item[String]("items.kind") != "potion") {
                        BadRequest(json.pretty(json.render(
                            ("why" -> s"Can't use an item like this.")
                        ))).as("application/json")
                    } else {
                        SQL("""DELETE FROM items
                                 WHERE id = {id}""").on(
                            "id" -> itemId
                        ).execute()

                        SQL("""UPDATE adventurers
                               SET hearts = {hearts}
                               WHERE id = {id}""").on(
                            "hearts" -> Math.min(adventurer[Int]("adventurers.hearts") + 5, 10), // XXX: 10 hearts is hardcoded
                            "id" -> adventurer[Int]("adventurers.id")
                        ).execute()

                        Ok(json.pretty(json.render(
                            ("why" -> s"You feel a little more invigorated.")
                        ))).as("application")
                    }
                }
            }
        }
    }

    def separate(adventurerName : String, itemId : Int) = Action { request =>
        DB.withTransaction { implicit c =>
            val adventurerOption = for {
                (username, password) <- util.getBasicAuth(request)
                row <- models.Adventurer.getAuthRow(username, password)
            } yield row

            (for {
                _ <- adventurerOption
                row <- models.Adventurer.getItem(itemId, adventurerName)
            } yield row) match {
                case None => {
                    NotFound(json.pretty(json.render(
                        ("why" -> s"You don't have this item anymore.")
                    ))).as("application/json")
                }
                case Some(item) => {
                    val attrs = json.parse(item[String]("items.attrs"))
                    implicit val formats = json.DefaultFormats

                    item[String]("items.kind") match {
                        case "weapon" => {
                            val class_ = (attrs \ "class").extract[String]

                            class_ match {
                                case "mace" | "spear" | "sword" => {
                                    val rest = (attrs \ "material").extract[String] match {
                                        case "wood" => "plank"
                                        case "iron" => "ingot"
                                        case "diamond" => "diamond"
                                    }

                                    SQL("""DELETE FROM items
                                           WHERE id = {id}""").on(
                                        "id" -> item[Int]("items.id")
                                    ).execute()

                                    Ok(json.pretty(json.render(
                                        "item_ids" -> (for {
                                            type_ <- List("stick", rest)
                                        } yield SQL("""INSERT INTO items (kind, owner_id, attrs)
                                                       VALUES ('part', {ownerId}, {attrs})""").on(
                                            "ownerId" -> item[Int]("items.owner_id"),
                                            "attrs" -> json.pretty(json.render(
                                                "type" -> type_
                                            ))
                                        ).executeInsert().get)
                                    ))).as("application/json")
                                }

                                case "ancient_staff" => {
                                    SQL("""DELETE FROM items
                                           WHERE id = {id}""").on(
                                        "id" -> item[Int]("items.id")
                                    ).execute()

                                    Ok(json.pretty(json.render(
                                        "item_ids" -> (for {
                                            type_ <- List("fire gem", "water gem", "earth gem", "air gem", "stick")
                                        } yield SQL("""INSERT INTO items (kind, owner_id, attrs)
                                                       VALUES ('part', {ownerId}, {attrs})""").on(
                                            "ownerId" -> item[Int]("items.owner_id"),
                                            "attrs" -> json.pretty(json.render(
                                                "type" -> type_
                                            ))
                                        ).executeInsert().get)
                                    ))).as("application/json")
                                }

                                case _ =>  BadRequest(json.pretty(json.render(
                                    ("why" -> s"Can't separate this item.")
                                ))).as("application/json")
                            }
                        }
                        case _ => BadRequest(json.pretty(json.render(
                            ("why" -> s"Can't separate this item.")
                        ))).as("application/json")
                    }
                }
            }
        }
    }
}
