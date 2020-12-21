name := "advent-of-code-2020"

version := "0.1"

scalaVersion := "2.13.4"

libraryDependencies ++= Seq(
  "com.github.pathikrit" %% "better-files" % "3.9.1",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.2.3" % Test,
)

//enablePlugins(ScalaNativePlugin)