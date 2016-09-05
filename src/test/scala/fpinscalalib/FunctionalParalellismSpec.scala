package fpinscalalib

import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import shapeless.HNil

class FunctionalParalellismSpec extends Spec with Checkers {
  def `par asyncF asserts` = {
    check(Test.testSuccess(FunctionalParallelismSection.parAsyncFAssert _, "10" :: HNil))
  }

  def `par sortPar asserts` = {
    FunctionalParallelismSection.parSortParAssert((l: List[Int]) => l.sorted)
  }

  def `par filter asserts` = {
    FunctionalParallelismSection.parFilterAssert(List(1, 2, 3))
  }

  def `par law mapping asserts` = {
    FunctionalParallelismSection.parLawMappingAssert(2)
  }

  def `par equal asserts` = {
    FunctionalParallelismSection.parEqualAssert(true)
  }

  def `par choiceN asserts` = {
    FunctionalParallelismSection.parChoiceNAssert(1)
  }

  def `par choiceMap asserts` = {
    FunctionalParallelismSection.parChoiceMapAssert(2)
  }

  def `par chooser asserts` = {
    FunctionalParallelismSection.parChooserAssert("odd")
  }

  def `par choice via flatMap asserts` = {
    FunctionalParallelismSection.parChoiceViaFlatMapAssert("a")
  }

  def `par choiceN via flatMap asserts` = {
    FunctionalParallelismSection.parChoiceNViaFlatMapAssert("c")
  }

  def `par flatMap and join asserts` = {
    FunctionalParallelismSection.parFlatMapJoinAssert("foo")
  }
}
