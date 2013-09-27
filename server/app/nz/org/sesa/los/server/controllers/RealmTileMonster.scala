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

object RealmTileMonster extends Controller {
    private def weaknessFor(name : String) = name match {
        case "ogre" => "sword"
        case "kobold" => "mace"
        case "elf" => "spear"
        case "dragon" => "ancient staff"
    }

    private def damageFor(material : String) = material match {
        case "wood" => 1
        case "iron" => 2
        case "diamond" => 4
        case "immaterial" => 8
    }

    private def attackMessageFor(class_ : String) = class_ match {
        case "mace" => "You bludgeon the monster with your mace"
        case "sword" => "You slice the monster with your sword"
        case "spear" => "You stab the monster with your spear"
        case "ancient staff" => "You cast a spell on the dragon with your staff"
    }

    private def getRow(realmName : String, x : Int, y : Int, monsterId : Int) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT *
                              FROM monsters, realms
                              WHERE monsters.realm_id = realms.id AND
                                    realms.name = {name} AND
                                    monsters.x = {x} AND
                                    monsters.y = {y} AND
                                    monsters.id = {monsterId}""").on(
                "name" -> realmName,
                "x" -> x,
                "y" -> y,
                "monsterId" -> monsterId
            )

            rows().toList match {
                case Nil => None
                case row::_ => Some(row)
            }
        }
    }

    def attack(realmName : String, x : Int, y : Int, monsterId : Int) = Action(parse.tolerantText) { request =>
        val adventurerOption = for {
            (username, password) <- util.getBasicAuth(request)
            row <- models.Adventurer.getAuthRow(username, password)
        } yield row

        (for {
            adventurer <- adventurerOption
            monster <- this.getRow(realmName, x, y, monsterId)
        } yield (adventurer, monster)) match {
            case None => {
                NotFound(json.pretty(json.render(
                    ("why" -> s"Er, that doesn't exist anymore.")
                ))).as("application/json")
            }

            case Some((adventurer, monster)) => {
                val js = json.parse(request.body)
                implicit val formats = json.DefaultFormats

                val weaponId = (js \ "weapon_id").extract[Int]

                models.Adventurer.getItem(weaponId, adventurer[String]("adventurers.name")) match {
                    case None => {
                        NotFound(json.pretty(json.render(
                            ("why" -> s"Er, that doesn't exist anymore.")
                        ))).as("application/json")
                    }

                    case Some(weapon) if weapon[String]("items.kind") != "weapon" => {
                        BadRequest(json.pretty(json.render(
                            ("why" -> s"That's not a weapon.")
                        ))).as("application/json")
                    }

                    case Some(weapon) => {
                        val attrs = json.parse(weapon[String]("items.attrs"))

                        val class_ = (attrs \ "class").extract[String]
                        val material = (attrs \ "material").extract[String]

                        if (class_ != this.weaknessFor(monster[String]("monsters.kind"))) {
                            Ok(json.pretty(json.render(
                                ("why" -> s"${this.attackMessageFor(class_)}. It didn't work...")
                            ))).as("application/json")
                        } else {
                            val hearts = monster[Int]("hearts") - this.damageFor(material)

                            if (hearts <= 0) {
                                DB.withTransaction { implicit c =>
                                    // give monster drops
                                    val json.JArray(drops) = json.parse(monster[String]("monsters.drops"))

                                    for {
                                        drop <- drops
                                    } SQL("""INSERT INTO items (kind, attrs, owner_id)
                                             VALUES ({kind}, {attrs}, {ownerId})""").on(
                                        "kind" -> (drop \ "kind").extract[String],
                                        "attrs" -> json.pretty(json.render(drop \ "attrs")),
                                        "ownerId" -> adventurer[Int]("adventurers.id")
                                    ).executeInsert()

                                    SQL("""DELETE FROM monsters
                                           WHERE id = {id}""").on(
                                        "id" -> monsterId
                                    ).execute()
                                }

                                Ok(json.pretty(json.render(
                                    ("why" -> s"${this.attackMessageFor(class_)} -- you slay the monster! You pick up some items that it dropped.")
                                ))).as("application/json")
                            } else {
                                DB.withTransaction { implicit c =>
                                    SQL("""UPDATE monsters
                                           SET hearts = {hearts}
                                           WHERE id = {id}""").on(
                                        "hearts" -> hearts,
                                        "id" -> monsterId
                                    ).execute()
                                }

                                Ok(json.pretty(json.render(
                                    ("why" -> s"${this.attackMessageFor(class_)} and deal ${this.damageFor(material)} heart${if (this.damageFor(material) > 1) "s" else ""} of damage.")
                                ))).as("application/json")
                            }
                        }
                    }
                }
            }
        }
    }
}
