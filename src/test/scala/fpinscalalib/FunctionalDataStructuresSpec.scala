package fpinscalalib

import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil

class FunctionalDataStructuresSpec extends Spec with Checkers {
  def `pattern matching 101 asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.patternMatching101Assert _, 42
        :: 1
        :: fpinscalalib.List(2, 3)
        :: HNil))
  }

  def `complex pattern matching asserts` = {
    check(Test.testSuccess(FunctionalDataStructuresSection.complexPatternAssert _, 3 :: HNil))
  }
}
