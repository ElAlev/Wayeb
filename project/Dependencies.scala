import sbt._

object Dependencies {

  object v {

    final val Logback = "1.2.3"
    final val ScalaLogging = "3.9.2"

    final val ScalaTest = "3.0.8"
    final val ScalaCheck = "1.14.1"
    final val JUnit = "4.13"

    final val Slick = "3.3.0"
    final val pSQL = "42.2.9"
    final val ScalaCSV = "1.3.6"
    final val JTS = "1.16.1"

    final val Breeze = "1.0"
    final val Smile = "1.5.3"

    final val Parser = "3.7.1"
    final val Config = "1.4.0"
    final val LearnLib = "0.15.0"

    final val Json = "2.7.4"
    final val Flink = "1.9.0"

  }

  // Logging using slf4j and logback
  lazy val Logging: Seq[ModuleID] = Seq(
    "ch.qos.logback" % "logback-classic" % v.Logback,
    "com.typesafe.scala-logging" %% "scala-logging" % v.ScalaLogging
  )

  // ScalaTest and ScalaCheck for UNIT testing
  lazy val Testing: Seq[ModuleID] = Seq(
    "org.scalatest" %% "scalatest" % v.ScalaTest % "test",
    "org.scalacheck" %% "scalacheck" % v.ScalaCheck % "test",
    "junit" % "junit" % v.JUnit
  )

  lazy val Data: Seq[ModuleID] = Seq(
    "com.typesafe.slick" %% "slick" % v.Slick,
    "com.typesafe.slick" %% "slick-hikaricp" % v.Slick,
    "org.postgresql" % "postgresql" % v.pSQL,
    "com.github.tototoshi" %% "scala-csv" % v.ScalaCSV
  )

  lazy val Math: Seq[ModuleID] = Seq(
    "org.scalanlp" %% "breeze" % v.Breeze,
    "org.scalanlp" %% "breeze-natives" % v.Breeze,
    "com.github.haifengl" %% "smile-scala" % v.Smile,
    "org.locationtech.jts" % "jts-core" % v.JTS
  )

  lazy val Tools: Seq[ModuleID] = Seq(
    "com.github.scopt" %% "scopt" % v.Parser,
    "com.typesafe" % "config" % v.Config,
    "de.learnlib" % "learnlib-rpni" % v.LearnLib,
    "com.typesafe.play" %% "play-json" % v.Json,
    "org.apache.flink" %% "flink-connector-kafka" % v.Flink
  )
}
