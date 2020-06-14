/*
 * Copyright 2016-2020 47 Degrees Open Source <https://www.47deg.com>
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
        .testSuccess(GettingStartedWithFPSection.isSortedAssert _, true :: false :: true :: HNil)
    )

  def `currying asserts`() =
    check(Test.testSuccess(GettingStartedWithFPSection.curryAssert _, true :: true :: HNil))

  def `uncurrying asserts`() =
    check(Test.testSuccess(GettingStartedWithFPSection.uncurryAssert _, true :: true :: HNil))

  def `composing asserts`() =
    check(Test.testSuccess(GettingStartedWithFPSection.composeAssert _, false :: 2 :: 3 :: HNil))
}
