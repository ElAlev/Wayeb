import Build.simAssemblySettings
import Build.cefAssemblySettings

addCommandAlias("build", ";compile;test;assembly")
addCommandAlias("rebuild", ";clean;build")

lazy val global = project
  .in(file("."))
  .disablePlugins(AssemblyPlugin)
  .aggregate(
    cef,
    sim
  )

lazy val cef = project
  .settings(name := "wayeb")
  .settings(logLevel in Test := Level.Info)
  .settings(logLevel in Compile := Level.Error)
  .settings(libraryDependencies ++= Dependencies.Logging)
  .settings(libraryDependencies ++= Dependencies.Testing)
  .settings(libraryDependencies ++= Dependencies.Data)
  .settings(libraryDependencies ++= Dependencies.Math)
  .settings(libraryDependencies ++= Dependencies.Tools)
  .settings(cefAssemblySettings)

lazy val sim = project
  .settings(name := "sim")
  .settings(logLevel in Test := Level.Info)
  .settings(logLevel in Compile := Level.Error)
  .settings(libraryDependencies ++= Dependencies.Logging)
  .settings(libraryDependencies ++= Dependencies.Tools)
  .settings(simAssemblySettings)
