package fpinscalalib

import fpinscalalib.customlib.laziness.Stream
import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil

class StrictnessAndLazinessSpec extends Spec with Checkers {
  import Gen.{const, sized, frequency, resize}
  import Arbitrary._

  implicit def arbStreamAlternative[T](implicit a: Arbitrary[T]): Arbitrary[Stream[T]] = {
    Arbitrary(sized(n =>
      frequency(
        (n, resize(n / 2, arbitrary[T]).map(Stream(_))),
        (1, const(Stream.empty)))))
  }

  def `if2 asserts` = {
    check(Test.testSuccess(StrictnessAndLazinessSection.if2Assert _, true :: HNil))
  }

  def `stream toList asserts` = {
    check(Test.testSuccess(StrictnessAndLazinessSection.streamToListAssert _, List(1, 2, 3) :: HNil))
  }

  def `stream take asserts` = {
    check(Test.testSuccess(StrictnessAndLazinessSection.streamTakeAssert _, 1 :: HNil))
  }

  def `stream drop asserts` = {
    check(Test.testSuccess(StrictnessAndLazinessSection.streamDropAssert _, 1 :: HNil))
  }

  def `stream takeWhile asserts` = {
    check(Test.testSuccess(StrictnessAndLazinessSection.streamTakeWhileAssert _, List(1, 2) :: List[Int]() :: HNil))
  }

  def `stream forAll asserts` = {
    check(Test.testSuccess(StrictnessAndLazinessSection.streamForAllAssert _, true :: HNil))
  }

  def `stream trace asserts` = {
    check(Test.testSuccess(StrictnessAndLazinessSection.streamTraceAssert _,
      11 :: Stream(2, 3, 4) :: Stream(3, 4) :: 13 :: Stream(4) :: 14 :: HNil))
  }
}
