package nz.org.sesa.los.server.models

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._
import java.sql.Connection

import nz.org.sesa.los.server.Position

object Adventurer {
    def getRow(adventurerName : String)(implicit c : Connection) = {
        val rows = SQL("""SELECT *
                          FROM adventurers, realms
                          WHERE adventurers.realm_id = realms.id AND
                                adventurers.name = {name}""").on(
            "name" -> adventurerName
        )

        rows().toList match {
            case Nil => None
            case row::_ => Some(row)
        }
    }

    def getAuthRow(adventurerName : String, token : String)(implicit c : Connection) = {
        val rows = SQL("""SELECT *
                          FROM adventurers, realms
                          WHERE adventurers.realm_id = realms.id AND
                                adventurers.name = {name} AND
                                adventurers.token = {token}""").on(
            "name" -> adventurerName,
            "token" -> token
        )

        rows().toList match {
            case Nil => None
            case row::_ => Some(row)
        }
    }

    def getItem(itemId : Int, owner : String)(implicit c : Connection) = {
        val rows = SQL("""SELECT *
                           FROM items, adventurers
                           WHERE items.id = {id} AND
                                 items.owner_id = adventurers.id AND
                                 adventurers.name = {owner}""").on(
            "id" -> itemId,
            "owner" -> owner
        )

        rows().toList match {
            case Nil => None
            case row::_ => Some(row)
        }
    }

    /// XXX: this type signature is ridiculous, like, really
    def moveDenialFor(pos : Position, target : Realm.Tile)(implicit c : Connection) = {
        val monsters = Realm.getMonsters(pos.realm, pos.x, pos.y)
        if (monsters.length > 0) {
            Some(if (monsters.length > 1) "Monsters block your path." else "A monster blocks your path.")
        } else {
            target.terrain match {
                case "river" | "lake" | "ocean" => Some("You try to flap your wings like a bird to fly over the water, but fail miserably.")
                case "lava" => Some("Um yeah, that's like, lava.")
                case "impassable" => Some("You walk into the wall and, to nobody's surprise, it hurts.")
                case _ => None
            }
        }
    }
}
