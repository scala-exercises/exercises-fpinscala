package fpinscalalib

import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil
import fpinscalalib.customlib.errorhandling._
import fpinscalalib.customlib.errorhandling.Option._
import fpinscalalib.customlib.errorhandling.Employee

class ErrorHandlingSpec extends Spec with Checkers {
  import Gen.{const, sized, frequency, resize}
  import Arbitrary._

  implicit def arbOptionAlternative[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] = {
    Arbitrary(sized(n =>
      frequency(
        (n, resize(n / 2, arbitrary[T]).map(Some(_))),
        (1, const(None)))))
  }

  implicit def arbOptionNil = Arbitrary(Some(Nil))

  def `option mean asserts` = {
    val none : Option[Double] = None

    check(Test.testSuccess(ErrorHandlingSection.optionMeanAssert _, none :: HNil))
  }

  def `option map asserts` = {
    val f = (e: Option[Employee]) => e.map(_.department)

    check(Test.testSuccess(ErrorHandlingSection.optionMapAssert _, f :: HNil))
  }

  def `option flatMap asserts` = {
    val f = (e: Option[Employee]) => e.flatMap(_.manager)

    check(Test.testSuccess(ErrorHandlingSection.optionFlatMapAssert _, f :: HNil))
  }

  def `option sequence asserts` = {
    val none : Option[List[Int]] = None
    check(Test.testSuccess(ErrorHandlingSection.optionSequenceAssert _, Some(List(1, 2, 3)) :: none :: HNil))
  }

  def `option traverse asserts` = {
    val none : Option[List[Int]] = None
    check(Test.testSuccess(ErrorHandlingSection.optionTraverseAssert _, Some(List(1, 2, 3)) :: none :: HNil))
  }

  // Some(List(1, 2, 3))
}
