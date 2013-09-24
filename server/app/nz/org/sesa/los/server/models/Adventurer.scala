package nz.org.sesa.los.server.models

import anorm._
import play.api.Play.current
import play.api._
import play.api.db._

object Adventurer {
    def getRow(adventurerName : String) = {
        DB.withConnection { implicit c =>
            val rows = SQL("""SELECT adventurers.id AS id,
                                     adventurers.name AS name,
                                     adventurers.level AS level,
                                     adventurers.x AS x,
                                     adventurers.y AS y,
                                     realms.name AS realm,
                                     realms.w AS w,
                                     realms.h AS h,
                                     adventurers.hp AS hp,
                                     adventurers.xp AS xp
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
    }

    def canMoveTo(adventurerRow : Row, tile : Realm.Tile) = {
        tile.terrain match {
            case "river" | "lake" | "ocean" => false
            case _ => true
        }
    }
}
