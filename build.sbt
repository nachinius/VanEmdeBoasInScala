name := "van emde boas"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

val scalaCheckVersion = "1.13.4"
libraryDependencies += "org.scalacheck" %% "scalacheck" % scalaCheckVersion
libraryDependencies += "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"

coverageEnabled := true
