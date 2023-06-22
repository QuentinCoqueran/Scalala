import sbt._

object Dependencies {
  lazy val betterFiles = "com.github.pathikrit" %% "better-files" % "3.9.2"
  lazy val typesafeConfig = "com.typesafe" % "config" % "1.4.2"
  lazy val playJson = "com.typesafe.play" %% "play-json" % "2.9.4"
  lazy val scalatic = "org.scalactic" %% "scalactic" % "3.2.16"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.16"
}
