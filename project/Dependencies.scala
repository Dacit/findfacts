import sbt._

object Dependencies {
  private val scalaVersion = "2.12.10"

  val solrVersion = "8.2.0"
  val playVersion = "2.7.3"

  val circeVersion = "0.12.0"
  val playCirceVersion = "2712.0"
  val playSwaggerVersion = "1.7.1"

  val log4jVersion = "2.12.1"
  val scalaTestVersion = "3.0.8"
  val scalaLoggingVersion = "3.9.2"

  val enumVersion = "1.5.13"
  val betterFilesVersion = "3.8.0"
  val wireVersion = "2.3.3"
  val scalaTestPlusVersion = "4.0.3"
  val shapelessVersion = "2.3.3"
  val catsVersion = "2.0.0"
  val scoptsVersion = "3.7.1"
  val swaggerUiVersion = "2.2.10"
  val parserCombinatorsVersion = "1.1.2"
  val fastparseVersion = "2.1.3"
  val tukaaniVersion = "1.8"

  val logging = "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion
  val loggingBackend = Seq(
    "org.apache.logging.log4j" % "log4j-slf4j-impl" % log4jVersion
      exclude ("org.slf4j", "slf4j-api"),
    "org.apache.logging.log4j" % "log4j-core" % log4jVersion
  )
  val enum = "com.beachape" %% "enumeratum" % enumVersion
  val files = "com.github.pathikrit" %% "better-files" % betterFilesVersion
  // Wire is only needed at compile-time
  val wire = "com.softwaremill.macwire" %% "macros" % wireVersion % "provided"
  val scalaTestBase = "org.scalatest" %% "scalatest" % scalaTestVersion
  val scalaTest = scalaTestBase
  val shapeless = "com.chuusai" %% "shapeless" % shapelessVersion
  val cats = "org.typelevel" %% "cats-core" % catsVersion
  val cmdOpts = "com.github.scopt" %% "scopt" % scoptsVersion
  val circeCore = "io.circe" %% "circe-core" % circeVersion
  val circeGeneric = "io.circe" %% "circe-generic" % circeVersion
  val circe = Seq(
    circeCore,
    circeGeneric,
    "io.circe" %% "circe-parser" % circeVersion
  )
  val scalaCompiler = "org.scala-lang" % "scala-compiler" % scalaVersion
  val swaggerUi = "org.webjars" % "swagger-ui" % swaggerUiVersion
  val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % parserCombinatorsVersion
  val fastParse = "com.lihaoyi" %% "fastparse" % fastparseVersion
  val solr = ("org.apache.solr" % "solr-core" % solrVersion
    exclude ("org.slf4j", "slf4j-api")
    exclude ("org.apache.logging.log4j", "log4j-api")
    exclude ("org.apache.logging.log4j", "log4j-web")
    exclude ("org.apache.logging.log4j", "log4j-core")
    exclude ("org.apache.logging.log4j", "log4j-slf4j-impl"))
  val playGuice = ("com.typesafe.play" %% "play-guice" % playVersion
    exclude ("org.slf4j", "slf4j-api"))
  val playCirce = "com.dripower" %% "play-circe" % playCirceVersion
  val playTestPlus = ("org.scalatestplus.play" %% "scalatestplus-play" % scalaTestPlusVersion
    exclude ("org.slf4j", "slf4j-api"))
  val playSwagger = ("io.swagger" %% "swagger-play2" % playSwaggerVersion
    exclude ("com.google.guava", "guava")
    exclude ("com.typesafe.play", "play-logback"))
  val isabelleDependencies = Seq("org.tukaani" % "xz" % tukaaniVersion)
}