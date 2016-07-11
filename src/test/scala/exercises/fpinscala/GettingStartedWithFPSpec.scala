package exercises

import fpinscalalib._
import fpinscalalib.GettingStartedWithFPSection
import shapeless.HNil
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}

class GettingStartedWithFPSpec extends Spec with Checkers {
  def `fibonacci asserts` = {
    implicit val intArbitrary = Arbitrary[Int](Gen.choose(1, 100))
    check(Test.testSuccess(GettingStartedWithFPSection.fibAssert _, 0 :: 1 :: HNil))
  }

  def `isSorted asserts` = {
    check(Test.testSuccess(GettingStartedWithFPSection.isSortedAssert _, true :: true :: true :: HNil))
  }
}
