// You have been provided with a piece of paper. This is your username for this
// tutorial.
//
// So, to get started, do the following (but replace your username with the
// username you have).
val me = login("username")

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
val map = retrieve("map")

// Okay, that's cool. Let's use our map.
map.use()

// Hmm...
map.use[List[Tile]]()
map.use[List[Tile]]().head

// Whoa, there's just a list of tiles. Let's store this somewhere.
val tiles = map.use[List[Tile]]().head

// Let's get one tile from it.
tiles(0)

// Hooray! Let's take the legend out of our inventory too.
val legend = retrieve("map legend")

// Let's try using this legend.
legend.use()
legend.use[String]()
legend.use[String](tiles(0)).head

// Hey, we get a colored tile! We can just go through all the tiles and apply
// the legend to them, right?
tiles.length

// Okay, that's a really big map. Why don't we just get the tiles around me?
me.x
me.y

def aroundMe(range: Int)(tile: Tile) =
    tile.x >= me.x - range && tile.x <= me.x + range && tile.y >= me.y - range && tile.y <= me.y + range

// Let's have a go writing showMap.
tiles.filter(aroundMe(5))

// Maybe 25 tiles.
def showMap =
    print(tiles.filter(aroundMe(25)).map(legend.use[String](_).head).mkString)

// ... not quite.
def showMap = {
    var lastY = 0

    println(tiles.filter(aroundMe(25)).map(tile => {
        var s = legend.use[String](tile).head
        if (tile.y != lastY) {
            s = "\n" + s
        }
        lastY = tile.y
        s
    }).mkString)
}

// What if we wanted to see ourselves on the map?
def showTile(tile : Tile) = (tile.x, tile.y) match {
    case (x, y) if (x, y) == (me.x, me.y) => Markers.Me
    case _ => legend.use[String](tile).head
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
showMap
print(me.look)

// Eh, why don't we write a lambda function that shows us the map and prints
// out what we're looking at every time we move?
me.afterMove = () => {
    showMap
    print(me.look)
}

me.move("north")

// Typing that is cumbersome.
def w = me.move("north")
def wa = me.move("northwest")
def s = me.move("south")
def sa = me.move("southwest")
def d = me.move("east")
def wd = me.move("northeast")
def a = me.move("west")
def sd = me.move("southeast")

// Still cumbersome.
def rep(n : Int, f : () => _) =
    (1 to n).foreach(_ => f())

// Okay, cool. What's this beacon dealio in our inventory?
val beacon = retrieve("beacon")
beacon.examine

// Wow, what a lazy description. At least we know what it does now.
beacon.use()
beacon.use[List[Signal]]()

// Let's try find another adventurer.
def makeTarget(pred : Signal => Boolean) = () => {
    beacon.use[List[Signal]]().head.find(pred).fold (-1, -1) { signal =>
        (signal.x, signal.y)
    }
}

var target = makeTarget({ signal =>
    signal.kind == Signal.Adventurer && signal.name == "username2"
})

def showTile(tile : Tile, target : (Int, Int)) = (tile.x, tile.y) match {
    case (x, y) if (x, y) == target => Markers.Target
    case (x, y) if (x, y) == (me.x, me.y) => Markers.Me
    case _ => legend.use[String](tile).head
}

def showMap = {
    var lastY = 0
    var tgt = target()

    println(tiles.filter(aroundMe(25)).map(tile => {
        var s = showTile(tile, tgt)
        if (tile.y != lastY) {
            s = "\n" + s
        }
        lastY = tile.y
        s
    }).mkString)
}

// Use our new showMap function every time we move.
me.afterMove = () => {
    showMap
    print(me.look)
}

case class SwordMold(hilt: Item, blade: Item)
