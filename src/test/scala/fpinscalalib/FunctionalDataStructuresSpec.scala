package fpinscalalib

import fpinscalalib.customlib.functionaldatastructures.{Branch, Leaf}
import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil
import fpinscalalib.customlib.functionaldatastructures._

class FunctionalDataStructuresSpec extends Spec with Checkers {

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

  def `list foldLeft sum product length asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listFoldLeftSumProductLengthAssert _, 0 :: 1.0 :: 0 :: 1 :: HNil))
  }

  def `list foldLeft append asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listAppendAssert _,
      List(1, 2, 3, 1, 2) :: List(1, 2, 3) :: List(1, 2) :: List[Int]() :: HNil))
  }

  def `list add 1 asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listAdd1Assert _, 1 :: HNil))
  }

  def `list remove odds asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listRemoveOdds _, 2 :: 0 :: HNil))
  }

  def `list flatMap asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listFlatMapAssert _, List(1, 1, 2, 2, 3, 3) :: HNil))
  }

  def `list zipWith asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listZipWithAssert _,
      List("aA", "bB", "cC") :: List("14", "25", "36") :: HNil))
  }

  def `list hasSubsequence asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.listHasSubsequenceAssert _, true :: false :: true :: HNil))
  }

  def `tree size asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.treeSizeAssert _, 1 :: 1 :: HNil))
  }

  def `tree depth asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.treeDepthAssert _, 0 :: 1 :: HNil))
  }

  def `tree map asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.treeMapAssert _,
      Branch[Int](Branch[Int](Leaf[Int](2), Leaf[Int](4)), Leaf[Int](6)) :: HNil))
  }

  def `tree fold asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.treeFoldAssert _,
      1 :: 1 :: 0 :: 1 :: Branch(Branch(Leaf(false), Leaf(true)), Leaf(false)) :: HNil))
  }
}
