val me = los.login("username")

def retrieve(name: String) =
    me.inventory.find (_.name == name).head

val tiles = retrieve("paper map").use().asInstanceOf[List[los.Tile]]
val legend = retrieve("map legend")

def nearMe(range: Int)(tile: los.Tile) =
    tile.x >= me.x - range && tile.x <= me.x + range && tile.y >= me.y - range && tile.y <= me.y + range

def showMap = {
    var lastY = 0

    print(tiles.filter(nearMe(25)).map(tile => {
        var s = legend.use(tile.terrain)
        if (tile.y != lastY) {
            s = "\n" + s
        }
        lastY = tile.y
        s
    }).mkString)
}

def showMap = {
    var lastY = 0

    print(tiles.filter(nearMe(25)).map(tile => {
        var s = (tile.x, tile.y) match {
            case (x, y) if x == me.x && y == me.y => los.Markers.Me
            case _ => legend.use(tile.terrain)
        }
        if (tile.y != lastY) {
            s = "\n" + s
        }
        lastY = tile.y
        s
    }).mkString)
}

me.move("north")

me.afterMove = () => {
    print(me.look)
    showMap
}

me.move("north")

def n = me.move("north")
def s = me.move("south")
def e = me.move("east")
def w = me.move("west")
