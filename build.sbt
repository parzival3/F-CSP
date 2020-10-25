name := "csp"

ThisBuild / version := "0.1"

ThisBuild / scalaVersion := "2.13.3"

ThisBuild / scalacOptions --= Seq("-Wunused:nowarn")

lazy val svmacros: Project = (project in file("svmacros")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  scalacOptions += "-Ymacro-annotations"
)

lazy val root = (project in file(".")).settings(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test",
).aggregate(svmacros).dependsOn(svmacros)