scalaVersion := "2.10.2"

scalacOptions += "-deprecation"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
  "net.liftweb" %% "lift-json" % "2.5.1",
  "org.slf4j" % "slf4j-simple" % "1.6.4"
)
