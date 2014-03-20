package nz.org.sesa.los.client

import nz.org.sesa.los.client.util._

import dispatch._, Defaults._
import net.liftweb.json
import net.liftweb.json.Serialization._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.runtime.universe.{TypeTag, typeTag}

case class Monster(val id : Int, val kind : String, val hearts : Int, val maxHearts : Int) {
    def name = kind
    def image = io.Source.fromInputStream(this.getClass.getResourceAsStream(s"/images/${this.name}.txt")).mkString
    def weakness = kind match {
        case "ogre" => "sword"
        case "kobold" => "mace"
        case "elf" => "spear"
        case "dragon" => "ancient staff"
    }

    override def toString = s"""
${this.image}
${Display.StartHilight}.name =${Display.Reset} ${name}
${Display.StartHilight}.hearts =${Display.Reset} ${Display.Bold}${Display.fg(196)}${(0 until this.hearts).map({_ => "♥"}).mkString(" ")}${Display.Reset} ${(this.hearts until this.maxHearts).map({_ => "♡"}).mkString(" ")}
${Display.StartHilight}.weakness =${Display.Reset} ${this.weakness}
"""
}
