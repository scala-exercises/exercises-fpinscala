package fpinscalalib

import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil

class FunctionalStateSpec extends Spec with Checkers {
  def `random non-negative integers asserts` = {
    FunctionalStateSection.randomNonNegativeIntAssert(0, 1)
  }

  def `random doubles asserts` = {
    FunctionalStateSection.randomDoubleAssert(1)
  }

  def `random integers list asserts` = {
    FunctionalStateSection.randomIntListAssert(0, 1)
  }

  def `random not negative even integer asserts` = {
    FunctionalStateSection.randomNonNegativeEvenAssert(2)
  }

  def `random doubles via map asserts` = {
    FunctionalStateSection.randomDoubleViaMap(1)
  }

  def `random non negative less than asserts` = {
    FunctionalStateSection.randomNonNegativeLessThan(1, 0)
  }

  def `random roll die asserts` = {
    FunctionalStateSection.randomRollDie(1)
  }

  def `candy machine asserts` = {
    FunctionalStateSection.candyMachineAssert(1, 1)
  }
}
