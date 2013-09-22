import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "los-server"
  val appVersion      = "1.0-SNAPSHOT"
  val scalaVersion    = "2.10.2"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "net.liftweb" %% "lift-json" % "2.5.1"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here
  )

}
