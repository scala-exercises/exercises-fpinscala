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
import org.scalaexercises.Test
import org.scalatest.refspec.RefSpec
import org.scalatestplus.scalacheck.Checkers
import shapeless.HNil
import fpinscalalib.customlib.errorhandling._
import fpinscalalib.customlib.errorhandling.Employee
import fpinscalalib.customlib.errorhandling.ExampleHelper._
import org.scalacheck.{Arbitrary, Gen}

class ErrorHandlingSpec extends RefSpec with Checkers {

  def `option map asserts`() = {
    val f = (e: Option[Employee]) => e.map(_.department)

    check(Test.testSuccess(ErrorHandlingSection.optionMapAssert _, f :: HNil))
  }

  def `option flatMap asserts`() = {
    val f = (e: Option[Employee]) => e.flatMap(_.manager)

    check(Test.testSuccess(ErrorHandlingSection.optionFlatMapAssert _, f :: HNil))
  }

  def `option orElse asserts`() = {
    check(
      Test.testSuccess(
        ErrorHandlingSection.optionOrElseAssert _,
        Some("Julie") :: Some("Mr. CEO") :: Some("Mr. CEO") :: HNil
      )
    )
  }

  def `option filter asserts`() = {

    implicit def optionArbitrary[T](implicit GT: Gen[T]): Arbitrary[Option[T]] =
      Arbitrary {
        Gen.option[T](GT) map {
          case scala.Some(v) => Some(v)
          case _             => None
        }
      }

    implicit val employeeArbitrary: Arbitrary[Employee] = Arbitrary {
      for {
        name       <- Gen.identifier
        department <- Gen.identifier
        manager    <- optionArbitrary(Gen.identifier).arbitrary
      } yield Employee(name, department, manager)
    }

    val none: Option[Employee] = None
    check(
      Test
        .testSuccess(ErrorHandlingSection.optionFilterAssert _, Some(joe) :: none :: none :: HNil)
    )
  }

  def `option sequence asserts`() = {
    val none: Option[List[Int]] = None
    check(
      Test.testSuccess(
        ErrorHandlingSection.optionSequenceAssert _,
        Some(List(1, 2, 3)) :: none :: HNil
      )
    )
  }

  def `option traverse asserts`() = {
    val none: Option[List[Int]] = None
    check(
      Test.testSuccess(
        ErrorHandlingSection.optionTraverseAssert _,
        Some(List(1, 2, 3)) :: none :: HNil
      )
    )
  }

  def `either map asserts`() = {
    val f = (e: Either[String, Employee]) => e.map(_.department)

    check(Test.testSuccess(ErrorHandlingSection.eitherMapAssert _, f :: HNil))
  }

  def `either flatMap asserts`() = {
    check(
      Test.testSuccess(
        ErrorHandlingSection.eitherFlatMapAssert _,
        Right("Julie") :: Left("Manager not found") :: Left("Employee not found") :: HNil
      )
    )
  }

  def `either orElse asserts`() = {
    check(
      Test.testSuccess(
        ErrorHandlingSection.eitherOrElseAssert _,
        Right("Julie") :: Right("Mr. CEO") :: Right("Mr. CEO") :: HNil
      )
    )
  }

  def `either map2 asserts`() = {
    check(
      Test.testSuccess(
        ErrorHandlingSection.eitherMap2Assert _,
        Right(false) :: Right(true) :: Left("Employee not found") :: HNil
      )
    )
  }

  def `either traverse asserts`() = {
    val list = List(joe, mary)
    check(
      Test.testSuccess(
        ErrorHandlingSection.eitherTraverseAssert _,
        Right(list) :: Left("Employee not found") :: HNil
      )
    )
  }

  def `either sequence asserts`() = {
    val list = List(joe, mary)
    check(
      Test.testSuccess(
        ErrorHandlingSection.eitherSequenceAssert _,
        Right(list) :: Left("Employee not found") :: HNil
      )
    )
  }
}
