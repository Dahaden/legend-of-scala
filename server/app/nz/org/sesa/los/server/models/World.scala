package nz.org.sesa.los.server.models

import net.liftweb.json

object World {
    case class Tile(val terrain : String, val features : List[String])

    def loadFromFile(filename : String) = {
        implicit val formats = json.DefaultFormats
        new World(json.parse(io.Source.fromFile(filename).mkString).extract[List[World.Tile]])
    }
}

class World(val tiles : List[World.Tile]) {
}
