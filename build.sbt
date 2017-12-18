name := "van emde boas"

version := "1.0"

scalaVersion := "2.12.4"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.4"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"

val scalaCheckVersion = "1.13.4"
libraryDependencies += "org.scalacheck" %% "scalacheck" % scalaCheckVersion
libraryDependencies += "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"

coverageEnabled := true



lazy val Benchmark = config("bench") extend Test

/**  This allows runneng ScalaMeter benchmarks in separate sbt configuration.
  *  It means, that when you want run your benchmarks you should type `bench:test` in sbt console.
  */
lazy val basic = Project(
  "basic-with-separate-config",
  file("."),
  settings = Defaults.coreDefaultSettings ++ Seq(
    name := "scalameter-examples",
    organization := "com.storm-enroute",
    scalaVersion := "2.12.4",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint"),
    publishArtifact := false,
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.8.2" % "bench"
    ),
    resolvers ++= Seq(
      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
      "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Benchmark := false,
    logBuffered := false
  )
) configs(
  Benchmark
  ) settings(
  inConfig(Benchmark)(Defaults.testSettings): _*
  )