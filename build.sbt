val refinedVersion = "0.9.18"
val zioVersion = "1.0.9"
val zioConfigVersion = "1.0.6"
val zioHttpVersion = "1.0.0.0-RC17"

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
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-config" % zioConfigVersion,
      "dev.zio" %% "zio-config-magnolia" % zioConfigVersion,
      "dev.zio" %% "zio-config-refined" % zioConfigVersion,
      "dev.zio" %% "zio-config-typesafe" % zioConfigVersion,
      "eu.timepit" %% "refined" % refinedVersion,
      "io.d11" %% "zhttp" % zioHttpVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test
    ),
    testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
  )

