val username = "nachinius"
val repo = "VanEmdeBoasInScala"

val scalaTestVersion = "3.0.4"
val scalaCheckVersion = "1.13.4"

name := "van Emde Boas Tree"
organization := "com.nachinius"
description := "A van Emde Boas data strucuture that achieves O(lg lg n) predecessor and successor queries with O(n) space"
scalaVersion := "2.12.4"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
)
homepage := Some(url(s"https://github.com/$username/$repo"))
licenses += "Apache License 2.0" -> url(s"https://github.com/$username/$repo/blob/master/LICENSE")
scmInfo := Some(ScmInfo(url(s"https://github.com/$username/$repo"), s"git@github.com:$username/$repo.git"))
apiURL := Some(url(s"https://$username.github.io/$repo/latest/api/"))
releaseCrossBuild := true
releasePublishArtifactsAction := PgpKeys.publishSigned.value
developers := List(
  Developer(id = username, name = "Ignacio `nachinius` Peixoto", email = "ignacio.peixoto@gmail.com", url = new URL(s"http://github.com/${username}"))
)
publishMavenStyle := true
publishArtifact in Test := true
publishTo := Some(if (isSnapshot.value) Opts.resolver.sonatypeSnapshots else Opts.resolver.sonatypeStaging)


lazy val Benchmark = config("bench") extend Test
/** This allows run ScalaMeter benchmarks in separate sbt configuration.
  * It means, that when you want run your benchmarks you should type `bench:test` in sbt console.
  */
lazy val basic = Project(
  "basic-with-separate-config",
  file("."),
  settings = Defaults.coreDefaultSettings ++ Seq(
    name := "benchmark-van-emde-boas",
    organization := "com.nachinius",
    scalaVersion := "2.12.4",
    scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-Xlint"),
    publishArtifact := false,
    libraryDependencies ++= Seq(
      "com.storm-enroute" %% "scalameter" % "0.8.2" % "bench"
    ),
//    resolvers ++= Seq(
//      "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
//      "Sonatype OSS Releases" at "https://oss.sonatype.org/content/repositories/releases"
//    ),
    testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework"),
    parallelExecution in Benchmark := false,
    logBuffered := false
  )
) configs (
  Benchmark
  ) settings (
  inConfig(Benchmark)(Defaults.testSettings): _*
  )
