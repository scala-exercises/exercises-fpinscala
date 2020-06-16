package fpinscalalib

import org.scalatest.refspec.RefSpec
import org.scalatest.prop.Checkers

class MonoidsSpec extends RefSpec with Checkers {
  def `monoid instances asserts` = {
    MonoidsSection.monoidInstancesAssert(0, 1, false, true)
    MonoidsSection.optionMonoidAssert(Option(2), Option(2))
    MonoidsSection.endoMonoidAssert(21, 22, 11, 10)
  }
}
