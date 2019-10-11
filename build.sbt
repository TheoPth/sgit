name := "sgit"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.8"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test"

target in assembly := file("../testSgit")
mainClass in assembly := Some("Main")
assemblyJarName in assembly := "sgit.jar"

libraryDependencies += "com.github.pathikrit" %% "better-files" % "3.8.0"

libraryDependencies +=  "org.json4s" %% "json4s-native" % "3.6.7"

libraryDependencies += "org.json4s" %% "json4s-ext" % "3.6.7"
