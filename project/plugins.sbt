addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")

/** Scalafmt - Code formatter for Scala **/
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.2")

/** Assembly - Create a fat JAR of your project with all of its dependencies **/
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.0.0")

/** Native Packager - Build application packages in native formats **/
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.8.1")

addSbtPlugin("au.com.onegeek" % "sbt-dotenv" % "2.1.204")

/** Code generation based on gql schemas. **/
addSbtPlugin("com.github.ghostdogpr" % "caliban-codegen-sbt" % "1.3.0")
