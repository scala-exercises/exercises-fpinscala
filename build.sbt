val scalaExerciesV = "0.4.0-SNAPSHOT"

def dep(artifactId: String) = "org.scala-exercises" %% artifactId % scalaExerciesV

lazy val fpinscala = (project in file("."))
  .enablePlugins(ExerciseCompilerPlugin)
  .settings(
    name         := "exercises-fpinscala",
    libraryDependencies ++= Seq(
      dep("exercise-compiler"),
      dep("definitions"),
      %%("scalatest"),
      %%("scalacheck"),
      %%("scheckShapeless")
    )
  )

// Distribution

pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.gpg")
pgpSecretRing := file(s"$gpgFolder/secring.gpg")
