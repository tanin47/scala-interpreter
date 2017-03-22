lazy val interpreter = (project in file("."))
  .settings(
    name := "interpreter",
    version := "1.0",
    scalaVersion := "2.11.8"
  )

resolvers ++= Seq("Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots")

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "3.0.1" % Test,
  "org.mockito" % "mockito-core" % "2.+" % Test,
  "org.specs2" %% "specs2-core" % "3.8.9" % Test,
  "org.specs2" %% "specs2-mock" % "3.8.9" % Test
)