ThisBuild / scalaVersion     := "2.13.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "krvoje"
ThisBuild / organizationName := "asciimap"

lazy val root = (project in file("."))
  .settings(
    name := "ascii-path",
    libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.7" % "test"
  ).settings(
    assemblyJarName in assembly := "ascii-map.jar",
  )

testFrameworks += new TestFramework("utest.runner.Framework")
