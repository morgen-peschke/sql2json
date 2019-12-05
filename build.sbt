val dottyVersion = "0.20.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    scalaVersion := dottyVersion,
    resolvers += "mvnrepository" at "http://mvnrepository.com/artifact/",
    libraryDependencies ++= Seq(
      "com.typesafe" % "config" % "1.4.0",
      "org.postgresql" % "postgresql" % "42.2.8",
      "mysql" % "mysql-connector-java" % "8.0.18",
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "net.java" % "quickcheck" % "0.6" % "test"
    )
  )
