/*
 *  scala-exercises - exercises-fpinscala
 *  Copyright (C) 2015-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
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
