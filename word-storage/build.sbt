ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "word-storage",
    resolvers += "mvn proxy" at "https://nexus.tcsbank.ru/repository/mvn-maven-proxy",
    libraryDependencies += "org.scala-lang" %% "scala3-library" % "3.0.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % Test,
    Compile / mainClass := Some(
      "ru.tinkoff.backendacademy.wordstorage.ConsoleInMemory"
    ),
    coverageFailOnMinimum := true,
    coverageMinimumStmtTotal := 60
  )
  .enablePlugins(JavaAppPackaging)

scalacOptions ++= Seq("-Werror")
