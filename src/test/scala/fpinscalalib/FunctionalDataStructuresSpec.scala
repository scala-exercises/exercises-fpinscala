package fpinscalalib

import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil

class FunctionalDataStructuresSpec extends Spec with Checkers {
  def `pattern matching 101 asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.patternMatching101Assert _,
      42 :: 1 :: fpinscalalib.List(2, 3) :: HNil))
  }

  def `complex pattern matching asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.complexPatternAssert _, 3 :: HNil))
  }

  def `list take asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listTakeAssert _, List(2, 3) :: List[Int]() :: HNil))
  }

  def `list setHead asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listSetHeadAssert _, List(3, 2, 3) :: List("c", "b") :: HNil))
  }

  def `list drop asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listDropAssert _,
      List(2, 3) :: List(1, 2, 3) :: List[Int]() :: List[Int]() :: List[Int]() :: HNil))
  }

  def `list dropWhile asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listDropWhileAssert _,
      List(2, 3) :: List(1, 2, 3) :: List[Int]() :: List[Int]() :: HNil))
  }

  def `list init asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listInitAssert _,
      List(1, 2) :: List[Int]() :: HNil))
  }

  def `list foldRight sum asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listFoldRightSumAssert _,
      1 :: 1 :: 2 :: 1 :: 2 :: 3 :: List[Int]() :: 1 :: 2 :: 3 :: 0 :: HNil))
  }

  def `list foldRight cons nil asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listFoldRightNilConsAssert _, List(1, 2, 3) :: HNil))
  }

  def `list length with foldRight asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listLengthAssert _, 0 :: 1 :: HNil))
  }
}
