package fpinscalalib

import org.scalaexercises.definitions._

/** Exercises based on Manning's "Functional Programming in Scala" by by Paul Chiusano and Rúnar Bjarnason.
  * For more information about the book please visit its
  * <a href="https://www.manning.com/books/functional-programming-in-scala">official website</a>.
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
    StrictnessAndLazinessSection,
    FunctionalStateSection,
    FunctionalParallelismSection
  )

  override def logoPath = "fp_in_scala"
}
