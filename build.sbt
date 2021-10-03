import com.typesafe.sbt.packager.docker._

val datastoreVersion  = "2.1.2"
val jwtVersion        = "8.0.2"
val refinedVersion    = "0.9.27"
val sttpClientVersion = "3.3.14"
val zioVersion        = "1.0.12"
val zioConfigVersion  = "1.0.10"
val zioHttpVersion    = "1.0.0.0-RC17+12-2f7aa146-SNAPSHOT"
val zioJsonVersion    = "0.1.5"
val zioLoggingVersion = "0.5.12"
val zioPreludeVersion = "1.0.0-RC5"

val root = (project in file("."))
  .settings(
    scalacOptions ++= Seq("-Ymacro-annotations"),
    inThisBuild(
      List(
        organization := "io.softwarechain",
        scalaVersion := "2.13.6",
        idePackagePrefix := Some("io.softwarechain.cryptojournal")
      )
    ),
    name := "crypto-journal-api",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    libraryDependencies ++= Seq(
      "com.google.cloud"              % "google-cloud-datastore"  % datastoreVersion,
      "com.softwaremill.sttp.client3" %% "core"                   % sttpClientVersion,
      "com.softwaremill.sttp.client3" %% "httpclient-backend-zio" % sttpClientVersion,
      "dev.zio"                       %% "zio"                    % zioVersion,
      "dev.zio"                       %% "zio-config"             % zioConfigVersion,
      "dev.zio"                       %% "zio-config-magnolia"    % zioConfigVersion,
      "dev.zio"                       %% "zio-config-refined"     % zioConfigVersion,
      "dev.zio"                       %% "zio-config-typesafe"    % zioConfigVersion,
      "dev.zio"                       %% "zio-json"               % zioJsonVersion,
      "dev.zio"                       %% "zio-logging"            % zioLoggingVersion,
      "dev.zio"                       %% "zio-logging-slf4j"      % zioLoggingVersion,
      "dev.zio"                       %% "zio-prelude"            % zioPreludeVersion,
      "eu.timepit"                    %% "refined"                % refinedVersion,
      "io.d11"                        %% "zhttp"                  % zioHttpVersion,
      "ch.qos.logback"                % "logback-classic"         % "1.2.6",
      "org.reactivestreams"           % "reactive-streams"        % "1.0.3",
      "com.github.jwt-scala"          %% "jwt-core"               % "9.0.1", //Use this until I figure out how to configure API-Gw with CloudRun
      "com.auth0"                     % "jwks-rsa"                % "0.19.0",
      "dev.zio"                       %% "zio-test"               % zioVersion % Test,
      "dev.zio"                       %% "zio-test-sbt"           % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
    assemblyMergeStrategy := {
      case "application.conf"                                            => MergeStrategy.concat
      case "module-info.class"                                           => MergeStrategy.discard
      case PathList("reactive-streams-flow-adapters-1.0.2.jar", xs @ _*) => MergeStrategy.discard
      case PathList("org", "reactivestreams", ps @ _*)                   => MergeStrategy.last
      case x =>
        val oldStrategy = assemblyMergeStrategy.value
        oldStrategy(x)
    },
    dockerBaseImage := "openjdk:18-slim",
    dockerExposedPorts := Seq(8080)
//    dockerCommands := dockerCommands.value.flatMap{
//      case cmd@Cmd("FROM", _) => List(cmd, Cmd("RUN", "apk update && apk add bash"))
//      case other => List(other)
//    }
  )
  .enablePlugins(JavaAppPackaging, DockerPlugin)

addCommandAlias("fmt", ";scalafmtAll;scalafmtSbt")
