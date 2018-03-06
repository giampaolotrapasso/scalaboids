name := "scalaboidsproject"

organization := "com.giampaolotrapasso"

version := "1.0"

scalaVersion := "2.12.3"


lazy val scalaboids = project.in(file("scalaboids"))

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies += "org.scalafx" %% "scalafx" % "8.0.102-R11"
  )
  .aggregate(scalaboids)
  .dependsOn(scalaboids)

// Fork a new JVM for 'run' and 'test:run' to avoid JavaFX double initialization problems
fork := true




