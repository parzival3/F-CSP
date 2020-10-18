name := "csp"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.0" % "test"
scalacOptions --= Seq("-Wunused:nowarn")
