name := "sgit"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

target in assembly := file("../testSgit")
assemblyJarName in assembly := "something.jar"