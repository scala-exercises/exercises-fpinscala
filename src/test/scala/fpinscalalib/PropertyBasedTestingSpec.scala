/*
 * Copyright 2016-2020 47 Degrees <https://47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
      Test.testSuccess(PropertyBasedTestingSection.genListOfN _, 10 :: List(42, 42, 42) :: HNil)
    )

  def `gen listOfN via flatMap`() =
    check(
      Test.testSuccess(
        PropertyBasedTestingSection.genListOfNViaFlatMap _,
        0 :: 10 :: List(42) :: HNil
      )
    )

  def `prop and or`() =
    PropertyBasedTestingSection.propAndOrAssert(Passed)

  def `sgen listOf`() =
    check(Test.testSuccess(PropertyBasedTestingSection.sGenListOfAssert _, 42 :: HNil))

  def `sgen listOf1`() =
    PropertyBasedTestingSection.sGenListOf1(1)

  def `prop takeWhile dropWhile`() =
    PropertyBasedTestingSection.propTakeWhileDropWhile(Passed)
}
