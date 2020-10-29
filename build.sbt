name := "csp"

ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.13.3"

ThisBuild / scalacOptions --= Seq("-Wunused:nowarn")

ThisBuild / libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"

lazy val svexamples: Project = (project in file("svexamples")).aggregate(root).dependsOn(root)

lazy val root = (project in file(".")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  scalacOptions += "-Ymacro-annotations",
)