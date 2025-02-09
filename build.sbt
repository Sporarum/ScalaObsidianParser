val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "obsidianparser",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "fastparse" % "3.1.1",
      "org.scalameta" %% "munit" % "1.0.0" % Test,
    )
  )
