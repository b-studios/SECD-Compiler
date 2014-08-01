scalaVersion in ThisBuild := "2.11.2"

scalacOptions in ThisBuild := Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

scalacOptions in ThisBuild ++= Seq("-optimize")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value
