name := "hello-world"
organization := "ch.epfl.scala"
version := "1.0"


addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.3")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")


val commonSettings = Seq(
  scalaVersion := "2.12.7",
  scalacOptions ++= Seq("-feature"),
  libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0",
  libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.5",
  libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
)

lazy val macroLib = project.in(file("src/main/scala/parser/core/macros")).settings(commonSettings : _*)
lazy val topApp = project.in(file(".")).settings(commonSettings : _*).dependsOn(macroLib)
