package fpinscalalib

import org.scalaexercises.definitions._

/** Set of exercises based on Manning's "Functional Programming in Scala" (aka "The Red Book")
  *
  * @param name fp_in_scala
  */
object FPinScalaLibrary extends Library {
  override def owner = "scala-exercises"
  override def repository = "exercises-fpinscala"

  override def color = Some("#E22D34")

  override def sections = scala.collection.immutable.List(
    GettingStartedWithFPSection,
    FunctionalDataStructuresSection,
    ErrorHandlingSection,
    StrictnessAndLazinessSection
  )
}
