ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "Advent-of-Code-2024",
    libraryDependencies ++= testDeps
  )

val testDeps = Seq("org.scalatestplus" %% "scalatestplus-mockito" % "1.0.0-M2").map(_ % "test")
