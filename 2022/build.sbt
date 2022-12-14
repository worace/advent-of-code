scalaVersion := "2.13.8"

name := "aoc2022"
organization := "works.worace"
version := "1.0"

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "0.7.5" % Test
)

testFrameworks += new TestFramework("munit.Framework"),

Global / onChangedBuildSource := ReloadOnSourceChanges
