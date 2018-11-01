lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      scalaVersion := "2.12.6",
      version := "0.1.0-SNAPSHOT"
    )),
  name := "raicoin",
  libraryDependencies ++= Seq(
    "org.scalatest"      %% "scalatest"    % "3.0.5",
    "com.typesafe.akka"  %% "akka-actor"   % "2.5.12",
    "com.typesafe.akka"  %% "akka-testkit" % "2.5.12" % Test,
    "commons-codec"      % "commons-codec" % "1.11",
    "org.scorexfoundation" %% "scrypto" % "2.1.2",
    "org.apache.commons" % "commons-lang3" % "3.7",
    "commons-io" % "commons-io" % "2.5"
  ),
  scalacOptions += "-feature",
  parallelExecution in Test := false,
  fork := true,
  testOptions in Test += Tests.Argument("-oD")
)
