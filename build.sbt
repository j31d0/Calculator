val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Calculator",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,


    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
    libraryDependencies += "org.jline" % "jline" % "3.21.0",
    libraryDependencies += "org.jline" % "jline-terminal-jna" % "3.21.0"
    )
