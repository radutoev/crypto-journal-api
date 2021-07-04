addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")
//addSbtPlugin("dev.zio" % "zio-shield" % "5cc58429308eb524b562eef71be584e508ee71f7")

/** Scalafmt - Code formatter for Scala **/
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.3.2")

/** Assembly - Create a fat JAR of your project with all of its dependencies **/
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.0.0")

/** Native Packager - Build application packages in native formats **/
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.8.1")