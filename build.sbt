name := "repair-maxsat"

version := "1.0"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-unchecked", "-deprecation")

javacOptions ++= Seq("-Xlint:unchecked")

compileOrder := CompileOrder.JavaThenScala

conflictManager := ConflictManager.strict

libraryDependencies ++= Seq("org.scalatest" % "scalatest_2.10" % "2.1.0" % "test",
                            "io.argonaut" %% "argonaut" % "6.0.3",
                            "junit" % "junit" % "4.9" % "test",
                            "com.novocode" % "junit-interface" % "0.10" % "test",
                            "com.google.guava" % "guava" % "13.0",
                            "com.github.scopt" %% "scopt" % "3.2.0")

resolvers += Resolver.sonatypeRepo("public")
