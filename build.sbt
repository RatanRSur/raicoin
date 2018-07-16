import Dependencies._

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      scalaVersion := "2.12.5",
      version := "0.1.0-SNAPSHOT"
    )),
  name := "blockchain",
  libraryDependencies ++= Seq(
    "org.scalatest"      %% "scalatest"    % "3.0.5",
    "com.typesafe.akka"  %% "akka-actor"   % "2.5.12",
    "com.typesafe.akka"  %% "akka-testkit" % "2.5.12" % Test,
    "commons-codec"      % "commons-codec" % "1.11",
    "org.apache.commons" % "commons-lang3" % "3.7"
  ),
  scalacOptions += "-feature",
  parallelExecution in Test := false,
  fork := true,
  testOptions in Test += Tests.Argument("-oD")
)
