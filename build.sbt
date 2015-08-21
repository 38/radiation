organization := "com.github._38"

name := "radiation"

version := "0.1"

//scalaVersion := "2.11.7"

//crossScalaVersions := Seq("2.10.2", "2.10.3", "2.10.4", "2.11.0", "2.11.1")

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies ++= Seq(
    "org.mozilla"    %    "rhino"    %    "1.7R5"
)

//addCompilerPlugin("org.scalamacros" % "paradise" % "2.0.1" cross CrossVersion.full)

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalaSource in Compile := baseDirectory.value / "src"
