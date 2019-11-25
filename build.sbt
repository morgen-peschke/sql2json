val dottyVersion = "0.20.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-simple",
    version := "0.1.0",
    //scalacOptions := Seq("-Xprint:frontend"),
    scalaVersion := dottyVersion,
    resolvers += "mvnrepository" at "http://mvnrepository.com/artifact/",
    libraryDependencies ++= Seq(
      "org.postgresql" % "postgresql" % "42.2.8",
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "net.java" % "quickcheck" % "0.6" % "test"
    )
  )
