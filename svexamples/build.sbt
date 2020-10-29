name := "svCspTests"

lazy val csp: Project = (project in file("../")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  scalacOptions += "-Ymacro-annotations"
)

lazy val root = (project in file(".")).settings(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test",
).aggregate(csp).dependsOn(csp)