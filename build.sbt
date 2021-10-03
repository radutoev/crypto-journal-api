import com.typesafe.sbt.packager.docker._

val auth0Version          = "0.19.0"
val datastoreVersion      = "2.1.2"
val jwtVersion            = "9.0.1"
val logbackVersion        = "1.2.6"
val reactiveVersion       = "1.0.3"
val refinedVersion        = "0.9.27"
val pubsubVersion         = "1.0.4"
val spotifyFuturesVersion = "4.2.0"
val sttpClientVersion     = "3.3.14"
val web3jVersion          = "5.0.0"
val zioVersion            = "1.0.12"
val zioConfigVersion      = "1.0.10"
val zioInteropVersion     = "1.3.7"
val zioHttpVersion        = "1.0.0.0-RC17+12-2f7aa146-SNAPSHOT"
val zioJsonVersion        = "0.1.5"
val zioLoggingVersion     = "0.5.12"
val zioPreludeVersion     = "1.0.0-RC5"

lazy val commonSettings = Nil ++
  Seq(
    scalacOptions ++= Seq("-Ymacro-annotations"),
    Compile / javaSource := baseDirectory.value / "main/java",
    Compile / scalaSource := baseDirectory.value / "main/scala",
    Compile / resourceDirectory := baseDirectory.value / "main/resources",
    Test / scalaSource := baseDirectory.value / "test/scala",
    Test / resourceDirectory := baseDirectory.value / "test/resources",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
  ) ++
  inThisBuild(
    List(
      organization := "io.softwarechain",
      scalaVersion := "2.13.6",
      idePackagePrefix := Some("io.softwarechain.cryptojournal")
    )
  )

lazy val packageSettings = Nil ++
  Seq(
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
  )

lazy val shared = project
  .in(file("src/shared"))
  .settings(
    commonSettings ++ Seq(name := "shared"),
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.client3" %% "core"                   % sttpClientVersion,
      "com.softwaremill.sttp.client3" %% "httpclient-backend-zio" % sttpClientVersion,
      "dev.zio"                       %% "zio"                    % zioVersion,
      "dev.zio"                       %% "zio-logging"            % zioLoggingVersion,
      "dev.zio"                       %% "zio-logging-slf4j"      % zioLoggingVersion,
      "dev.zio"                       %% "zio-json"               % zioJsonVersion,
      "dev.zio"                       %% "zio-test"               % zioVersion % Test,
      "dev.zio"                       %% "zio-test-sbt"           % zioVersion % Test,
      "eu.timepit"                    %% "refined"                % refinedVersion
    )
  )

lazy val sync = project
  .in(file("src/sync"))
  .settings(
    commonSettings ++ packageSettings ++ Seq(name := "sync"),
    libraryDependencies ++= Seq(
      "ch.qos.logback"                % "logback-classic"              % logbackVersion,
      "com.google.cloud"              % "google-cloud-pubsublite"      % pubsubVersion,
      "com.spotify"                   % "futures-extra"                % spotifyFuturesVersion,
      "dev.zio"                       %% "zio-config-magnolia"         % zioConfigVersion,
      "dev.zio"                       %% "zio-config-refined"          % zioConfigVersion,
      "dev.zio"                       %% "zio-config-typesafe"         % zioConfigVersion,
      "dev.zio"                       %% "zio-interop-reactivestreams" % zioInteropVersion,
      "org.web3j"                     % "core"                         % web3jVersion
    )
  )
  .dependsOn(shared)
  .enablePlugins(JavaAppPackaging, DockerPlugin)

lazy val journal = project
  .in(file("src/journal"))
  .settings(
    commonSettings ++ packageSettings ++ Seq(name := "journal"),
    libraryDependencies ++= Seq(
      "com.auth0"                     % "jwks-rsa"                % auth0Version,
      "com.github.jwt-scala"          %% "jwt-core"               % jwtVersion,
      "com.google.cloud"              % "google-cloud-datastore"  % datastoreVersion,
      "dev.zio"                       %% "zio-config"             % zioConfigVersion,
      "dev.zio"                       %% "zio-config-magnolia"    % zioConfigVersion,
      "dev.zio"                       %% "zio-config-refined"     % zioConfigVersion,
      "dev.zio"                       %% "zio-config-typesafe"    % zioConfigVersion,
      "dev.zio"                       %% "zio-prelude"            % zioPreludeVersion,
      "io.d11"                        %% "zhttp"                  % zioHttpVersion,
      "ch.qos.logback"                % "logback-classic"         % logbackVersion,
      "org.reactivestreams"           % "reactive-streams"        % reactiveVersion,
    )
  )
  .dependsOn(shared)
  .enablePlugins(JavaAppPackaging, DockerPlugin)

addCommandAlias("fmt", ";scalafmtAll;scalafmtSbt")
