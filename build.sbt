name := "Evo-poker"

version := "0.1"

scalaVersion := "2.13.4"

/**
 *  IntelliJ doesn't have proper support for Coursier (which is strange),
 *  but sbt is using Coursier to download dependencies,
 *  then IntelliJ can't find them because it's looking in my ivy cache. facepalm
 *  adding this line solved the problem for me
 * */
ThisBuild / useCoursier := false

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.2"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"