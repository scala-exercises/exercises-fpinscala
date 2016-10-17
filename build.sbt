lazy val fpinscala = (project in file("."))
.settings(publishSettings:_*)
.enablePlugins(ExerciseCompilerPlugin)
.settings(
  organization := "org.scala-exercises",
  name         := "exercises-fpinscala",
  scalaVersion := "2.11.8",
  version := "0.3.0-SNAPSHOT",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  ),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.2.4",
    "org.scala-exercises" %% "exercise-compiler" % "0.2.5-SNAPSHOT",
    "org.scala-exercises" %% "definitions" % "0.2.5-SNAPSHOT",
    "org.scalacheck" %% "scalacheck" % "1.12.5",
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.12" % "0.3.1",
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0")
  )
)

// Distribution

lazy val gpgFolder = sys.env.getOrElse("SE_GPG_FOLDER", ".")

lazy val publishSettings = Seq(
  organizationName := "Scala Exercises",
  organizationHomepage := Some(new URL("http://scala-exercises.org")),
  startYear := Some(2016),
  description := "Scala Exercises: The path to enlightenment",
  homepage := Some(url("http://scala-exercises.org")),
  pgpPassphrase := Some(sys.env.getOrElse("SE_GPG_PASSPHRASE", "").toCharArray),
  pgpPublicRing := file(s"$gpgFolder/pubring.gpg"),
  pgpSecretRing := file(s"$gpgFolder/secring.gpg"),
  credentials += Credentials("Sonatype Nexus Repository Manager",  "oss.sonatype.org",  sys.env.getOrElse("PUBLISH_USERNAME", ""),  sys.env.getOrElse("PUBLISH_PASSWORD", "")),
  scmInfo := Some(ScmInfo(url("https://github.com/scala-exercises/exercises-fpinscala"), "https://github.com/scala-exercises/exercises-fpinscala.git")),
  licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra :=
      <developers>
        <developer>
          <id>jdesiloniz</id>
          <name>Javier de Silóniz</name>
          <email>javier.s.s@47deg.com</email>
        </developer>
        <developer>
          <id>juanpedromoreno</id>
          <name>Juan Pedro Moreno</name>
          <email>juanpedro.m@47deg.com</email>
        </developer>
      </developers>
)
