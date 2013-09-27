scalaVersion := "2.10.2"

scalacOptions ++= Seq(
    "-deprecation",
    "-feature"
)

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "net.databinder.dispatch" % "dispatch-core_2.10" % "0.11.0",
  "net.liftweb" % "lift-json_2.10" % "2.5.1",
  "org.slf4j" % "slf4j-simple" % "1.6.4"
)

traceLevel := 0
