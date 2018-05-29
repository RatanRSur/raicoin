import sbt._

object Dependencies {
  lazy val scalaTest     = "org.scalatest"      %% "scalatest"    % "3.0.5"
  lazy val akka          = "com.typesafe.akka"  %% "akka-actor"   % "2.5.12"
  lazy val akkaTest      = "com.typesafe.akka"  %% "akka-testkit" % "2.5.12" % Test
  lazy val apacheCommons = "org.apahce.commons" % "commons-codec" % "1.11"
}
