lazy val commonSettings = Seq(
  scalaVersion := "2.12.7",
  version := "0.1.0-SNAPSHOT",
  scalacOptions ++= Seq("-feature", "-deprecation"),
  testOptions in Test += Tests.Argument("-oD"),
)

lazy val raicoin = (project in file(".")).enablePlugins(DockerPlugin, DockerComposePlugin).settings(
  commonSettings,
  name := "raicoin",
  libraryDependencies ++= Seq(
    "org.scalatest"        %% "scalatest"    % "3.0.5",
    "com.typesafe.akka"    %% "akka-actor"   % "2.5.18",
    "com.typesafe.akka"    %% "akka-testkit" % "2.5.18",
    "commons-codec"        % "commons-codec" % "1.11",
    "org.scorexfoundation" %% "scrypto"      % "2.1.2",
    "org.apache.commons"   % "commons-lang3" % "3.8.1",
    "commons-io"           % "commons-io"    % "2.6",
    "com.github.scopt" %% "scopt" % "4.0.0-RC2"
  ),
  parallelExecution in Test := false,
  fork := true,
  connectInput in run := true,
  trapExit := false,
  outputStrategy := Some(StdoutOutput),
  mainClass in assembly := Some("raicoin.Raicoin"),
  test in assembly := {},
  assemblyMergeStrategy in assembly := {
      case "module-info.class" => MergeStrategy.rename
      case x => (assemblyMergeStrategy in assembly).value(x)
  },
  dockerImageCreationTask := docker.value,
  dockerfile in docker := {
      val artifact: File = assembly.value
      val artifactTargetPath = "."

      new Dockerfile {
        from("openjdk:11-jre")
        copy(artifact, artifactTargetPath)
        env("RUNTIME_ARGS" -> "")
        entryPoint("java", "-jar", artifactTargetPath, "${RUNTIME_ARGS}")
      }
  },
  imageNames in docker := Seq(ImageName(s"${name.value}"))
)

lazy val resourceGeneration = (project in file("resource-gen")).dependsOn(raicoin % "compile->test").settings(commonSettings)
