name := "fpinscala"

version := "0.1"

scalaVersion := "2.12.3"

lazy val chapter2 = project in file("chapter-2")

lazy val root = (project in file("."))
  .dependsOn(chapter2)