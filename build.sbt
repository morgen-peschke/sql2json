val dottyVersion = "0.20.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
      "org.postgresql" % "postgresql" % "42.2.8",
      "com.novocode" % "junit-interface" % "0.11" % "test"
    )
  )
