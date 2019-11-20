/*
 *  scala-exercises - exercises-fpinscala
 *  Copyright (C) 2015-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
 */

package fpinscalalib

import fpinscalalib.customlib.testing.Prop.Passed
import org.scalaexercises.Test
import org.scalacheck.ScalacheckShapeless._
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.Checkers
import shapeless.HNil

class PropertyBasedTestingSpec extends RefSpec with Checkers {

  def `prop and asserts`() =
    check(Test.testSuccess(PropertyBasedTestingSection.propAndAssert _, 4 :: 4 :: true :: HNil))

  def `gen choose int`() =
    PropertyBasedTestingSection.genChooseIntAssert(0, 0, 10, 10)

  def `gen unit`() =
    check(Test.testSuccess(PropertyBasedTestingSection.genUnitAssert _, 42 :: "foo" :: HNil))

  def `gen listOfN`() =
    check(
      Test.testSuccess(PropertyBasedTestingSection.genListOfN _, 10 :: List(42, 42, 42) :: HNil))

  def `gen listOfN via flatMap`() =
    check(
      Test.testSuccess(
        PropertyBasedTestingSection.genListOfNViaFlatMap _,
        0 :: 10 :: List(42) :: HNil))

  def `prop and or`() =
    PropertyBasedTestingSection.propAndOrAssert(Passed)

  def `sgen listOf`() =
    check(Test.testSuccess(PropertyBasedTestingSection.sGenListOfAssert _, 42 :: HNil))

  def `sgen listOf1`() =
    PropertyBasedTestingSection.sGenListOf1(1)

  def `prop takeWhile dropWhile`() =
    PropertyBasedTestingSection.propTakeWhileDropWhile(Passed)
}
