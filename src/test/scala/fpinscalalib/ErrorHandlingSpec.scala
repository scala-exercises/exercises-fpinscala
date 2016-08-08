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
      Some("Julie") :: Some("Mr. CEO") :: Some("Mr. CEO") :: HNil))
  }

  def `option filter asserts` = {
    val none : Option[Employee] = None
    check(Test.testSuccess(ErrorHandlingSection.optionFilterAssert _,
      Some(joe) :: none :: none :: HNil))
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
      Right("Julie") :: Left("Manager not found") :: Left("Employee not found") :: HNil))
  }

  def `either orElse asserts` = {
    check(Test.testSuccess(ErrorHandlingSection.eitherOrElseAssert _,
      Right("Julie") :: Right("Mr. CEO") :: Right("Mr. CEO") :: HNil))
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
