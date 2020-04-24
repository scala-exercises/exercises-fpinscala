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

import fpinscalalib.customlib.functionaldatastructures.{Branch, Leaf, _}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{const, frequency, resize, sized}
import org.scalacheck.ScalacheckShapeless._
import org.scalaexercises.Test
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.Checkers
import shapeless.HNil

class FunctionalDataStructuresSpec extends RefSpec with Checkers {

  implicit def arbListArbitrary[T](implicit a: Arbitrary[T]): Arbitrary[List[T]] =
    Arbitrary(sized(n => frequency((n, resize(n / 2, arbitrary[T]).map(List(_))), (1, const(Nil)))))

  def `complex pattern matching asserts`() =
    check(Test.testSuccess(FunctionalDataStructuresSection.complexPatternAssert _, 3 :: HNil))

  def `list take asserts`() =
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listTakeAssert _,
        List(2, 3) :: List[Int]() :: HNil
      )
    )

  def `list setHead asserts`() =
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listSetHeadAssert _,
        List(3, 2, 3) :: List("c", "b") :: HNil
      )
    )

  def `list drop asserts`() = {
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listDropAssert _,
        List(2, 3) :: List(1, 2, 3) :: List[Int]() :: List[Int]() :: List[Int]() :: HNil
      )
    )
  }

  def `list dropWhile asserts`() = {
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listDropWhileAssert _,
        List(2, 3) :: List(1, 2, 3) :: List[Int]() :: List[Int]() :: HNil
      )
    )
  }

  def `list init asserts`() = {
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listInitAssert _,
        List(1, 2) :: List[Int]() :: HNil
      )
    )
  }

  def `list foldRight sum asserts`() = {
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listFoldRightSumAssert _,
        1 :: 1 :: 2 :: 1 :: 2 :: 3 :: List[Int]() :: 1 :: 2 :: 3 :: 0 :: HNil
      )
    )
  }

  def `list foldRight cons nil asserts`() =
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listFoldRightNilConsAssert _,
        List(1, 2, 3) :: HNil
      )
    )

  def `list length with foldRight asserts`() =
    check(Test.testSuccess(FunctionalDataStructuresSection.listLengthAssert _, 0 :: 1 :: HNil))

  def `list foldLeft sum product length asserts`() =
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listFoldLeftSumProductLengthAssert _,
        0 :: 1.0 :: 0 :: 1 :: HNil
      )
    )

  def `list foldLeft append asserts`() = {
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listAppendAssert _,
        List(1, 2, 3, 1, 2) :: List(1, 2, 3) :: List(1, 2) :: List[Int]() :: HNil
      )
    )
  }

  def `list add 1 asserts`() =
    check(Test.testSuccess(FunctionalDataStructuresSection.listAdd1Assert _, 1 :: HNil))

  def `list remove odds asserts`() =
    check(Test.testSuccess(FunctionalDataStructuresSection.listRemoveOdds _, 2 :: 0 :: HNil))

  def `list flatMap asserts`() =
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listFlatMapAssert _,
        List(1, 1, 2, 2, 3, 3) :: HNil
      )
    )

  def `list zipWith asserts`() = {
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listZipWithAssert _,
        List("aA", "bB", "cC") :: List("14", "25", "36") :: HNil
      )
    )
  }

  def `list hasSubsequence asserts`() =
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.listHasSubsequenceAssert _,
        true :: false :: true :: HNil
      )
    )

  def `tree size asserts`() =
    check(Test.testSuccess(FunctionalDataStructuresSection.treeSizeAssert _, 1 :: 1 :: HNil))

  def `tree depth asserts`() =
    check(Test.testSuccess(FunctionalDataStructuresSection.treeDepthAssert _, 0 :: 1 :: HNil))

  def `tree map asserts`() = {
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.treeMapAssert _,
        Branch[Int](Branch[Int](Leaf[Int](2), Leaf[Int](4)), Leaf[Int](6)) :: HNil
      )
    )
  }

  def `tree fold asserts`() = {
    check(
      Test.testSuccess(
        FunctionalDataStructuresSection.treeFoldAssert _,
        1 :: 1 :: 0 :: 1 :: Branch(Branch(Leaf(false), Leaf(true)), Leaf(false)) :: HNil
      )
    )
  }
}
