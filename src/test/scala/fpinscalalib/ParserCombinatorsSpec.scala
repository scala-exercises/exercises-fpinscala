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
import org.scalacheck.ScalacheckShapeless._
import org.scalaexercises.Test
import shapeless.HNil

class ParserCombinatorsSpec extends RefSpec with Checkers {
  def `parser many1 asserts`() =
    ParserCombinatorsSection.parserMany1Assert("a", "aaa")

  def `parser many asserts`() =
    ParserCombinatorsSection.parserManyAssert(Right(List()), Right(List('a', 'a', 'a')))

  def `parser listOfN asserts`() =
    ParserCombinatorsSection.parserListOfNAssert(0, 1)

  def `parser flatMap asserts`() =
    ParserCombinatorsSection.parseFlatMapAssert("[0-9]+")

  def `parser string asserts`() =
    check(Test.testSuccess(ParserCombinatorsSection.parserStringAssert _, "42" :: HNil))

  def `parser regex asserts`() =
    ParserCombinatorsSection.parserRegexAssert("[0-9]+")

  def `parser slice asserts`() =
    check(Test.testSuccess(ParserCombinatorsSection.parserSliceAssert _, true :: HNil))

  def `parser json asserts`() =
    check(Test.testSuccess(ParserCombinatorsSection.parserJSONAssert _, "[]" :: HNil))
}
