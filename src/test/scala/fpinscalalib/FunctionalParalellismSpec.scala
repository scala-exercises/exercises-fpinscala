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
    FunctionalParallelismSection.parSortPar((l: List[Int]) => l.sorted)
  }
}
