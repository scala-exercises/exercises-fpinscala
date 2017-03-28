/*
 * scala-exercises - exercises-fpinscala
 * Copyright (C) 2015-2016 47 Degrees, LLC. <http://www.47deg.com>
 */

package fpinscalalib

import fpinscalalib.customlib.laziness.Stream
import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil

class StrictnessAndLazinessSpec extends Spec with Checkers {
  import Gen.{const, frequency, resize, sized}
  import Arbitrary._

  implicit def arbStreamAlternative[T](implicit a: Arbitrary[T]): Arbitrary[Stream[T]] = {
    Arbitrary(sized(n =>
      frequency((n, resize(n / 2, arbitrary[T]).map(Stream(_))), (1, const(Stream.empty)))))
  }

  def `stream toList asserts` =
    check(
      Test.testSuccess(StrictnessAndLazinessSection.streamToListAssert _, List(1, 2, 3) :: HNil))

  def `stream take asserts` =
    check(Test.testSuccess(StrictnessAndLazinessSection.streamTakeAssert _, 1 :: HNil))

  def `stream drop asserts` =
    check(Test.testSuccess(StrictnessAndLazinessSection.streamDropAssert _, 1 :: HNil))

  def `stream takeWhile asserts` =
    check(
      Test.testSuccess(
        StrictnessAndLazinessSection.streamTakeWhileAssert _,
        List(1, 2) :: List[Int]() :: HNil))

  def `stream forAll asserts` =
    check(Test.testSuccess(StrictnessAndLazinessSection.streamForAllAssert _, true :: HNil))

  def `stream trace asserts` = {
    check(
      Test.testSuccess(
        StrictnessAndLazinessSection.streamTraceAssert _,
        11 :: Stream(2, 3, 4) :: Stream(3, 4) :: 13 :: Stream(4) :: 14 :: HNil))
  }

  def `stream ones asserts` = {
    check(
      Test.testSuccess(
        StrictnessAndLazinessSection.streamOnesAssert _,
        List(1, 1, 1, 1, 1) :: true :: true :: false :: HNil))
  }

  def `stream integers asserts` =
    check(Test.testSuccess(StrictnessAndLazinessSection.streamIntegersAssert _, 1 :: HNil))

  def `stream fibs asserts` =
    check(Test.testSuccess(StrictnessAndLazinessSection.streamFibsAssert _, 0 :: 1 :: HNil))

  def `stream fibs via unfold asserts` =
    check(
      Test.testSuccess(StrictnessAndLazinessSection.streamFibsViaUnfoldAssert _, 0 :: 1 :: HNil))

  def `stream integers via unfold asserts` =
    check(Test.testSuccess(StrictnessAndLazinessSection.streamIntegersAssert _, 1 :: HNil))

  def `stream ones via unfold asserts` =
    StrictnessAndLazinessSection.streamOnesViaUnfoldAssert(Some(1, 1))

  def `stream take via unfold asserts` =
    StrictnessAndLazinessSection.streamTakeViaUnfold(0, 1)

  def `stream tails asserts` =
    StrictnessAndLazinessSection.streamTailsAssert(1)

  def `stream scanRight asserts` =
    check(
      Test.testSuccess(
        StrictnessAndLazinessSection.streamScanRightAssert _,
        List(6, 5, 3, 0) :: HNil))
}
