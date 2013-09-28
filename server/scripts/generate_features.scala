// run this script with :load in play console
import anorm._
import play.api.Play.current
import play.api.db._
import play.core.StaticApplication
import nz.org.sesa.los.server.models
import scala.util.Random

new StaticApplication(new java.io.File("."))

DB.withTransaction { implicit c =>
    val tiles = models.Realm.loadTiles("world").zipWithIndex

    // generate monsters on 70% of eligible tiles
    val rand = new Random(System.currentTimeMillis())
    val monsterLocs = rand.shuffle(tiles.filter({ case (tile, i) =>
        models.Realm.difficultyFor(tile.terrain) > 0
    }))

    println("generating monsters...")
    for {
        i <- 0 to (monsterLocs.length * 0.7).toInt
    } {
        val (tile, j) = monsterLocs(i)

        // XXX: LOL HARDCODED
        val x = j % 150
        val y = j / 150

        models.Realm.makeMonster("world", x, y, false)
    }

    // generate dungeons on 30% of eligible tiles
    val dungeonLocs = rand.shuffle(monsterLocs)

    println("generating dungeons...")
    for {
        i <- 0 to (dungeonLocs.length * 0.05).toInt
    } {
        val (tile, j) = dungeonLocs(i)

        // XXX: LOL HARDCODED
        val x = j % 150
        val y = j / 150

        models.Realm.makeRandomDungeonAt("world", x, y)
    }
}
