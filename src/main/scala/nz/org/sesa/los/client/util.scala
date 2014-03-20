package nz.org.sesa.los.client.util

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
    var Target = Display.Bold + Display.fg(0) + Display.bg(82) + "tg" + Display.Reset
}
