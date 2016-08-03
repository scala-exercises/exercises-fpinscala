package fpinscalalib

import org.scalacheck.Shapeless._
import org.scalacheck.{Arbitrary, Gen}
import org.scalaexercises.Test
import org.scalatest.Spec
import org.scalatest.prop.Checkers
import shapeless.HNil
import fpinscalalib.customlib.errorhandling._


class ErrorHandlingSpec extends Spec with Checkers {
  import Gen.{const, sized, frequency, resize}
  import Arbitrary._

  implicit def arbOptionAlternative[T](implicit a: Arbitrary[T]): Arbitrary[Option[T]] = {
    Arbitrary(sized(n =>
      frequency(
        (n, resize(n / 2, arbitrary[T]).map(Some(_))),
        (1, const(None)))))
  }


  def `option mean asserts` = {
    val foo : Option[Double] = None

    check(Test.testSuccess(ErrorHandlingSection.optionMeanAssert _, foo :: HNil))
  }
}
