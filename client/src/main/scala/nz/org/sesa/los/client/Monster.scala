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
    override def toString = s"""
${this.image}
${Display.StartHilight}.name =${Display.Reset} ${name}
${Display.Bold}${Display.fg(196)}.hearts = {(0 until this.hearts).map({_ => "♥"}).mkString(" ")}${Display.Reset} ${(this.hearts until this.maxHearts).map({_ => "♡"}).mkString(" ")}
"""
}
