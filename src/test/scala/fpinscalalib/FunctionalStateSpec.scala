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

import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.Checkers

class FunctionalStateSpec extends RefSpec with Checkers {
  def `random non-negative integers asserts`() =
    FunctionalStateSection.randomNonNegativeIntAssert(0, 1)

  def `random doubles asserts`() =
    FunctionalStateSection.randomDoubleAssert(1)

  def `random integers list asserts`() =
    FunctionalStateSection.randomIntListAssert(0, 1)

  def `random doubles via map asserts`() =
    FunctionalStateSection.randomDoubleViaMap(1)

  def `random non negative less than asserts`() =
    FunctionalStateSection.randomNonNegativeLessThan(1, 0)

  def `random roll die asserts`() =
    FunctionalStateSection.randomRollDie(1)

  def `candy machine asserts`() =
    FunctionalStateSection.candyMachineAssert(1, 1)
}
