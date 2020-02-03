/*
 *  scala-exercises - exercises-fpinscala
 *  Copyright (C) 2015-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
 */

package fpinscalalib

import org.scalacheck.ScalacheckShapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.Checkers
import shapeless._

class GettingStartedWithFPSpec extends RefSpec with Checkers {

  def `fibonacci asserts`() = {
    implicit val arb = Arbitrary {
      for {
        res0 <- Gen.choose(2, 10)
        res1 <- Gen.choose(2, 10)
      } yield res0 :: res1 :: HNil
    }

    check(Test.testSuccess(GettingStartedWithFPSection.fibAssert _, 0 :: HNil))
  }

  def `isSorted asserts`() =
    check(
      Test
        .testSuccess(GettingStartedWithFPSection.isSortedAssert _, true :: false :: true :: HNil))

  def `currying asserts`() =
    check(Test.testSuccess(GettingStartedWithFPSection.curryAssert _, true :: true :: HNil))

  def `uncurrying asserts`() =
    check(Test.testSuccess(GettingStartedWithFPSection.uncurryAssert _, true :: true :: HNil))

  def `composing asserts`() =
    check(Test.testSuccess(GettingStartedWithFPSection.composeAssert _, false :: 2 :: 3 :: HNil))
}
