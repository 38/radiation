name := "radiation"

scalaSource in Compile := baseDirectory.value / "src"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.mozilla"    %  "rhino"            % "1.7R4"
)
