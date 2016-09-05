package fpinscalalib

import org.scalaexercises.Test
import org.scalacheck.Shapeless._
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil

class PropertyBasedTestingSpec extends Spec with Checkers {
  def `prop and asserts` = {
    check(Test.testSuccess(PropertyBasedTestingSection.propAndAssert _, 4 :: 4 :: true :: HNil))
  }
}