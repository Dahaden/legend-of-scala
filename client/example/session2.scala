val me = login("test", "token")

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
    }
}

def autobattle = {
    while (me.look.monsters.length > 0) {
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

autobattle
me.move("north")
