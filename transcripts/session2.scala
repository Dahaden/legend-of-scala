val me = login("test3", "token")

case class SwordMold(hilt : Item, blade : Item)
case class SpearMold(pole : Item, tip : Item)
case class MaceMold(handle : Item, head : Item)

me.look.features(0).use
var weapon = me.combine(new SwordMold(me.inventory(0), me.inventory(1))).head
me.inventory(0).use(me.look.monsters(0))

def oppositeDirectionOf(d : String) = {
    d match {
        case "north" => "south"
        case "south" => "north"
        case "west" => "east"
        case "east" => "west"
        case "northeast" => "southwest"
        case "northwest" => "southeast"
        case "southeast" => "northwest"
        case "southwest" => "northeast"
    }
}

def autobattle = {
    while (me.look.monsters.length > 0) {
        if (me.hearts <= 5) {
            me.inventory.find(_.name == "potion").fold {()} {_.use}
        }

        val monster = me.look.monsters.head
        var parts = weapon.separate

        weapon = me.combine(monster.weakness match {
            case "sword" => new SwordMold(parts(0), parts(1))
            case "spear" => new SpearMold(parts(0), parts(1))
            case "mace" => new MaceMold(parts(0), parts(1))
        }).head

        weapon.use(monster)
    }
}

def autoexplore = {
    var trail : List[String] = Nil

    while (me.look.exits.length > 0) {
        val exits = me.look.exits
        val next = trail match {
            case Nil => exits.head
            case d::_ => exits.find({ d2 =>
                d != oppositeDirectionOf(d2)
            }).head
        }
        trail = next :: trail
        me.move(next)
        autobattle
    }
}

autoexplore
autobattle
me.look.features(0).use

def retrieve(name: String) =
    me.inventory.find (_.name == name).head

val map = retrieve("map")
val tiles = map.use[List[Tile]].head

val legend = retrieve("map legend")

def aroundMe(range: Int)(tile: Tile) =
    tile.pos.x >= me.pos.x - range && tile.pos.x <= me.pos.x + range && tile.pos.y >= me.pos.y - range && tile.pos.y <= me.pos.y + range

def showTile(tile : Tile) = tile.pos match {
    case pos if pos == me.pos => Markers.Me
    case _ => legend.use[String](tile).head
}

def showMap = {
    var lastY = 0

    println(tiles.filter(aroundMe(25)).map(tile => {
        var s = showTile(tile)
        if (tile.pos.y != lastY) {
            s = "\n" + s
        }
        lastY = tile.pos.y
        s
    }).mkString)
}

def move(d : String) = {
    me.move(d)
    showMap
    print(me.look)
    autobattle
}

def w = move("north")
def wa = move("northwest")
def s = move("south")
def sa = move("southwest")
def d = move("east")
def wd = move("northeast")
def a = move("west")
def sd = move("southeast")

// weapon reassembly
weapon = me.combine(new SwordMold(weapon.separate()(0), retrieve("ingot"))).head

