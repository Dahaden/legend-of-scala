package nz.org.sesa.los.client

import scala.io.Source

object Images {
    val Beacon = Source.fromFile("images/beacon.txt").mkString
    val Map = Source.fromFile("images/map.txt").mkString
    val Paper = Source.fromFile("images/legend.txt").mkString
    val LogoSplash = Source.fromFile("images/splash.txt").mkString
}
