package nz.org.sesa.los.server.models

import scala.collection.mutable

import anorm._
import net.liftweb.json
import play.api.Play.current
import play.api._
import play.api.db._

object Realm {
    def getRow(realmName : String) = {
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

    private val cache : mutable.Map[String, List[Tile]] = mutable.Map().withDefault(loadFromFile)

    case class Tile(val terrain : String, val features : List[String])

    private def loadFromFile(name : String) = {
        implicit val formats = json.DefaultFormats
        json.parse(io.Source.fromFile("maps/" + name + ".json").mkString).extract[List[Tile]]
    }

    def loadTiles(name : String) = {
        this.cache.getOrElseUpdate(name, {
            loadFromFile(name)
        })
    }

    def unloadTiles(name : String) = {
        this.cache -= name
    }
}

