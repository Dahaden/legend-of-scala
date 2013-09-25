package nz.org.sesa.los.server

case class Position(val x : Int, val y : Int, val realm : String) {
    override def toString = s"Position(.x = $x, .y = $y, .realm = $realm)"
}
