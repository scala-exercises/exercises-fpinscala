/*
 *  scala-exercises - exercises-fpinscala
 *  Copyright (C) 2015-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
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
