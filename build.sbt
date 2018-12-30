lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      scalaVersion := "2.12.7",
      version := "0.1.0-SNAPSHOT"
    )),
  name := "raicoin",
  libraryDependencies ++= Seq(
    "org.scalatest"        %% "scalatest"    % "3.0.5",
    "com.typesafe.akka"    %% "akka-actor"   % "2.5.18",
    "com.typesafe.akka"    %% "akka-testkit" % "2.5.18" % Test,
    "commons-codec"        % "commons-codec" % "1.11",
    "org.scorexfoundation" %% "scrypto"      % "2.1.2",
    "org.apache.commons"   % "commons-lang3" % "3.8.1",
    "commons-io"           % "commons-io"    % "2.6",
    "com.github.scopt" %% "scopt" % "4.0.0-RC2"
  ),
  scalacOptions ++= Seq("-feature", "-deprecation"),
  parallelExecution in Test := false,
  fork := true,
  connectInput in run := true,
  trapExit := false,
  outputStrategy := Some(StdoutOutput),
  testOptions in Test += Tests.Argument("-oD")
)
