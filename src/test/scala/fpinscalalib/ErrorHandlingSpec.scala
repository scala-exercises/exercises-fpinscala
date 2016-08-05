package fpinscalalib

import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil
import fpinscalalib.customlib.errorhandling._
import fpinscalalib.customlib.errorhandling.Employee
import fpinscalalib.customlib.errorhandling.ExampleHelper._

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

  def `option orElse asserts` = {
    check(Test.testSuccess(ErrorHandlingSection.optionOrElseAssert _,
      Some(1) :: Some(0) :: HNil))
  }

  def `option filter asserts` = {
    val none : Option[Int] = None
    check(Test.testSuccess(ErrorHandlingSection.optionFilterAssert _,
      Some(1) :: none :: HNil))
  }

  def `option sequence asserts` = {
    val none : Option[List[Int]] = None
    check(Test.testSuccess(ErrorHandlingSection.optionSequenceAssert _, Some(List(1, 2, 3)) :: none :: HNil))
  }

  def `option traverse asserts` = {
    val none : Option[List[Int]] = None
    check(Test.testSuccess(ErrorHandlingSection.optionTraverseAssert _, Some(List(1, 2, 3)) :: none :: HNil))
  }

  def `either mean asserts` = {
    check(Test.testSuccess(ErrorHandlingSection.eitherMeanAssert _, Right(3.0) :: Left("mean of empty list!") :: HNil))
  }

  def `either map asserts` = {
    val f = (e: Either[String, Employee]) => e.map(_.department)

    check(Test.testSuccess(ErrorHandlingSection.eitherMapAssert _, f :: HNil))
  }

  def `either flatMap asserts` = {
    check(Test.testSuccess(ErrorHandlingSection.eitherFlatMapAssert _,
      Right(5) :: Left("/ by zero") :: HNil))
  }

  def `either orElse asserts` = {
    check(Test.testSuccess(ErrorHandlingSection.eitherOrElseAssert _,
      Right(1) :: Left("Parsing error") :: Right(5) :: Left("Division by zero error") :: HNil))
  }

  def `either map2 asserts` = {
    check(Test.testSuccess(ErrorHandlingSection.eitherMap2Assert _,
      Right(false) :: Right(true) :: Left("Employee not found") :: HNil))
  }

  def `either traverse asserts` = {
    val list = List(joe, mary)
    check(Test.testSuccess(ErrorHandlingSection.eitherTraverseAssert _,
      Right(list) :: Left("Employee not found") :: HNil))
  }

  def `either sequence asserts` = {
    val list = List(joe, mary)
    check(Test.testSuccess(ErrorHandlingSection.eitherSequenceAssert _,
      Right(list) :: Left("Employee not found") :: HNil))
  }
}
