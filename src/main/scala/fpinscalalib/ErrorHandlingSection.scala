package fpinscalalib

import fpinscalalib.customlib.errorhandling.{Option, Some, None}
import org.scalatest.{FlatSpec, Matchers}

/** @param name handling_error_without_exceptions
  */
object ErrorHandlingSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {
  /**
    * = The Option data type =
    *
    * Exceptions break referential transparency and introduce context dependence. Moreover, they are not type-safe,
    * hiding information about the fact that they may occur, to the developer and the compiler. We're going to explore
    * an alternative to exceptions without these drawbacks, without losing out on the primary benefit of exceptions:
    * they allow us to `consolidate and centralize error-handling logic`. The technique we use is based on an old idea:
    * instead of throwing an exception, we return a value indicating that an exceptional condition has occurred. instead
    * of using error codes, we introduce a new generic type for these “possibly defined values” and use higher-order
    * functions to encapsulate common patterns of handling and propagating errors.
    *
    * We introduce a new type, `Option`. As with the previously explored `List`, this type also exists in the Scala
    * standard library, but we're re-creating it here for pedagogical purposes:
    *
    * {{{
    *   sealed trait Option[+A]
    *   case class Some[+A](get: A) extends Option[A]
    *   case object None extends Option[Nothing]
    * }}}
    *
    * Option has two cases: it can be defined, in which case it will be a `Some`, or it can be undefined, in which case
    * it will be `None`.
    *
    * Let's consider an example to use our new type. We're defining a function `mean` that computes the mean of a list,
    * which is undefined if the list is empty:
    */

  def optionMeanAssert(res0: Option[Double]): Unit = {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    mean(Seq(1, 2, 3, 4, 5)) shouldBe Some(3.0)
    mean(Seq.empty) shouldBe res0
  }
}
