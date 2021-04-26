name := "fpinscala"

version := "0.1"

scalaVersion := "2.13.5"

lazy val ch2 = (project in file("ch2"))
lazy val ch3 = (project in file("ch3"))

lazy val root = (project in file("."))
  .dependsOn(ch2)
  .dependsOn(ch3)

Global / onChangedBuildSource := ReloadOnSourceChanges
