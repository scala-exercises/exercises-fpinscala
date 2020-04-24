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

import org.scalaexercises.Test
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.Checkers
import org.scalacheck.ScalacheckShapeless._
import shapeless.HNil

class FunctionalParalellismSpec extends RefSpec with Checkers {
  def `par asyncF asserts`() =
    check(Test.testSuccess(FunctionalParallelismSection.parAsyncFAssert _, "10" :: HNil))

  def `par filter asserts`() =
    FunctionalParallelismSection.parFilterAssert(List(1, 2, 3))

  def `par choiceN asserts`() =
    FunctionalParallelismSection.parChoiceNAssert(1)

  def `par choiceMap asserts`() =
    FunctionalParallelismSection.parChoiceMapAssert(2)

  def `par chooser asserts`() =
    FunctionalParallelismSection.parChooserAssert("odd")

  def `par choice via flatMap asserts`() =
    FunctionalParallelismSection.parChoiceViaFlatMapAssert("a")

  def `par choiceN via flatMap asserts`() =
    FunctionalParallelismSection.parChoiceNViaFlatMapAssert("c")

  def `par flatMap and join asserts`() =
    FunctionalParallelismSection.parFlatMapJoinAssert("foo")
}
