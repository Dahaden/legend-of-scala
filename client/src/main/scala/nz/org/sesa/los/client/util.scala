package nz.org.sesa.los.client.util

sealed class DefaultsTo[A, B]
trait LowPriorityDefaultsTo {
    implicit def overrideDefault[A,B] = new DefaultsTo[A,B]
}
object DefaultsTo extends LowPriorityDefaultsTo {
    implicit def default[B] = new DefaultsTo[B, B]
}
class Defaults[B] {
    type To[A] = DefaultsTo[A,B]
}

object Display {
    val Reset = 27.toChar + "[0m"
    val Bold = 27.toChar + "[1m"
    def fg(c: Int) = 27.toChar + s"[38;5;${c}m"
    def bg(c: Int) = 27.toChar + s"[48;5;${c}m"

    val StartHilight = s"${fg(255)}$Bold"
    def show(x: String) = println(s"${Display.StartHilight}$x${Display.Reset}\n")
}

object Markers {
    val Me = Display.Bold + Display.fg(0) + Display.bg(226) + "u!" + Display.Reset
    var Player = Display.Bold + Display.fg(0) + Display.bg(82) + "pl" + Display.Reset
}

object Features {
    val Dungeon = "dungeon"
}
