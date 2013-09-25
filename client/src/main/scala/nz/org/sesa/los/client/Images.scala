package nz.org.sesa.los.client

import scala.io.Source

object Images {
    val Beacon = Source.fromInputStream(this.getClass.getResourceAsStream("/images/beacon.txt")).mkString
    val Map = Source.fromInputStream(this.getClass.getResourceAsStream("/images/map.txt")).mkString
    val Paper = Source.fromInputStream(this.getClass.getResourceAsStream("/images/legend.txt")).mkString
    val Chest = Source.fromInputStream(this.getClass.getResourceAsStream("/images/chest.txt")).mkString
    val LogoSplash = Source.fromInputStream(this.getClass.getResourceAsStream("/images/splash.txt")).mkString
}
