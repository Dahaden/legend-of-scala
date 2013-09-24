package nz.org.sesa.los.server.models

import net.liftweb.json

object Realm {
    private val cache : Map[String, List[Tile]] = Map().withDefault(loadFromFile)

    case class Tile(val terrain : String, val features : List[String])

    private def loadFromFile(name : String) = {
        implicit val formats = json.DefaultFormats
        json.parse(io.Source.fromFile("maps/" + name + ".json").mkString).extract[List[Tile]]
    }

    def loadTiles(name : String) = {
        this.cache(name)
    }
}

