import sbt.Keys._
import sbt.{AutoPlugin, _}
import sbt.plugins.JvmPlugin
import sbtassembly.AssemblyPlugin
import sbtassembly.AssemblyPlugin.autoImport._

object Build extends AutoPlugin {

  private val logger = ConsoleLogger()

  override def requires: Plugins = JvmPlugin && AssemblyPlugin
  override def trigger: PluginTrigger = allRequirements

  override def projectSettings: Seq[Setting[_]] = settings

  val javaVersion: Double = sys.props("java.specification.version").toDouble

  private lazy val settings: Seq[Setting[_]] = {
    logger.info(s"[info] Loading settings for Java $javaVersion or higher.")
    if (javaVersion < 1.8) sys.error("Java 8 or higher is required for building Wayeb.")
    else commonSettings ++ assemblySettings
  }

  private val commonSettings: Seq[Setting[_]] = Seq(

    name := "Wayeb",

    organization := "com.gitlab.elias",

    description := "A forecasting engine for Scala",

    scalaVersion := "2.12.10",

    crossScalaVersions := Seq("2.12.10", "2.11.12"),

    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.typesafeRepo("releases"),
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),

    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
    ),

    dependencyOverrides ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value,
      "org.scala-lang" % "scala-library" % scalaVersion.value,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2",
      "org.scala-lang.modules" %% "scala-xml" % "1.2.0"
    )
  )

  private lazy val assemblySettings: Seq[Setting[_]] = Seq(

    //mainClass in assembly := Some("ui.WayebCLI"),

    test in assembly := { },

    assemblyJarName in assembly := s"${name.value.toLowerCase}-${version.value}.jar",

    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", _ @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  )


  lazy val simAssemblySettings: Seq[Setting[_]] = Seq(

    mainClass in assembly := Some("StreamSimulator"),

    assemblyJarName in assembly := s"${name.value.toLowerCase}-${version.value}.jar",

    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", _ @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  )

  lazy val cefAssemblySettings: Seq[Setting[_]] = Seq(

    mainClass in assembly := Some("ui.WayebCLI"),

    test in assembly := { },

    assemblyJarName in assembly := s"${name.value.toLowerCase}-${version.value}.jar",

    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", _ @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  )
}
