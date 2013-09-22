// You have been provided with a piece of paper. This is your username for this
// tutorial.
//
// So, to get started, do the following (but replace your username with the
// username you have).
val me = los.login("username")

// Okay, you seem to be in some sort of game. Why don't we take a look around?
me.look

// Hm, okay. Do we have an inventory?
me.inventory

// Huh, we have a map. That seems useful. It's the first item in our inventory.
// Let's examine it.
me.inventory(0).examine

// Okay. Hey, we also have a legend!
me.inventory(1).examine

// Hm, using indexes for these things is kind of cumbersome. We can get the
// names of things in our inventory.
me.inventory(0).name

// Why don't we write a function to get items in our inventory?
def retrieve(name: String) =
    me.inventory.find (_.name == name).head

// Neat, now let's retrieve our map.
val map = retrieve("paper map")

// Okay, that's cool. Let's use our map.
map.use()

// Whoa, there's just a list of tiles. Let's store this somewhere.
val tiles = map.use()

// Let's get one tile from it.
tiles(0)

// Hm, that's not the right type.
val tiles = map.use().asInstanceOf[List[los.Tile]]
tiles(0)

// Hooray! Let's take the legend out of our inventory too.
val legend = retrieve("map legend")

// What happens when we use the legend on a map tile?
legend.use(tiles(0))

// Hey, we get a colored tile! We can just go through all the tiles and apply
// the legend to them, right?
tiles.length

// Okay, that's a really big map. Why don't we just get the tiles around me?
me.x
me.y

def aroundMe(range: Int)(tile: los.Tile) =
    tile.x >= me.x - range && tile.x <= me.x + range && tile.y >= me.y - range && tile.y <= me.y + range

// Let's have a go writing showMap.
tiles.filter(aroundMe(5))

// Maybe 25 tiles.
def showMap =
    print(tiles.filter(aroundMe(25)).map(legend.use(_)).mkString)

// ... not quite.
def showMap = {
    var lastY = 0

    println(tiles.filter(aroundMe(25)).map(tile => {
        var s = legend.use(tile)
        if (tile.y != lastY) {
            s = "\n" + s
        }
        lastY = tile.y
        s
    }).mkString)
}

// What if we wanted to see ourselves on the map?
var showTile = (tile : los.Tile) => (tile.x, tile.y) match {
    case (x, y) if x == me.x && y == me.y => los.Markers.Me
    case _ => legend.use(tile)
}

def showMap = {
    var lastY = 0

    println(tiles.filter(aroundMe(25)).map(tile => {
        var s = showTile(tile)
        if (tile.y != lastY) {
            s = "\n" + s
        }
        lastY = tile.y
        s
    }).mkString)
}

// Now that we have a map, let's go exploring.
me.move("north")
print(me.look)
showMap

// Eh, why don't we write a lambda function that shows us the map and prints
// out what we're looking at every time we move?
me.afterMove = () => {
    print(me.look)
    showMap
}

me.move("north")

// Typing that is cumbersome.
def n = me.move("north")
def s = me.move("south")
def e = me.move("east")
def w = me.move("west")

// Still cumbersome.
def ni(i: Int) : Bool = i match { case 0 => true; case i => { if (n) ni(i - 1) else false } }
def si(i: Int) : Bool = i match { case 0 => true; case i => { if (s) si(i - 1) else false } }
def ei(i: Int) : Bool = i match { case 0 => true; case i => { if (e) ei(i - 1) else false } }
def wi(i: Int) : Bool = i match { case 0 => true; case i => { if (w) wi(i - 1) else false } }

// Okay, cool. What's this beacon dealio in our inventory?
val beacon = retrieve("handheld beacon")
beacon.examine

// Wow, what a lazy description. At least we know what it does now.
beacon.use()

// Let's try find another player.
def players = beacon.use("players").asInstanceOf[List[los.Coordinate]]
showTile = (tile : los.Tile) => (tile.x, tile.y) match {
    case (x, y) if x == me.x && y == me.y => los.Markers.Me
    case (x, y) if players contains (x, y) => los.Markers.Player
    case _ => legend.use(tile)
}
