organization := "io.github.radiation"

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
    "org.mozilla"    %    "rhino"    %    "1.7.7"
)

libraryDependencies += "com.github.scopt" %% "scopt" % "3.3.0"

resolvers += Resolver.sonatypeRepo("public")

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalaSource in Compile := baseDirectory.value / "src"
