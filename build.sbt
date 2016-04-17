// Common
scalaVersion := "2.11.8"
scalacOptions += "-deprecation"

// Project
name := "exercises"
version := "1.0.0-SNAPSHOT"
organization := "fpinscala"

// Code Formatting
// scalariformSettings

// Libraries
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % Test withSources(),
  "org.scalacheck" %% "scalacheck" % "1.12.5" % Test withSources())
