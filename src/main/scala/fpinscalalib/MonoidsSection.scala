package fpinscalalib

import fpinscalalib.customlib.monoids.Monoid
import org.scalatest.Matchers
import org.scalatest.FlatSpec

object MonoidsSection extends FlatSpec
  with Matchers
  with org.scalaexercises.definitions.Section {

  /**
    * = Functional programming in Scala =
    *
    * The following set of sections represent the exercises contained in the book "Functional Programming in Scala",
    * written by Paul Chiusano and Rúnar Bjarnason and published by Manning. This content library is meant to be used
    * in tandem with the book. We use the same numeration for the exercises for you to follow them.
    *
    * For more information about "Functional Programming in Scala" please visit its
    * <a href="https://www.manning.com/books/functional-programming-in-scala">official website</a>.
    *
    * = What is a monoid? =
    *
    * <b>Exercise 10.1</b>
    *
    * Give Monoid instances for integer addition and multiplication as well as the Boolean operators
    *
    *
    * Let's implement Monoid instances for integer addition and  multiplication as well as the Boolean operators, taking this representation of `Monoid`:
    *
    * {{{
    *   trait Monoid[A] {
    *     def op(a1: A, a2: A): A
    *     def zero: A
    *   }
    * }}}
    **/
  def monoidInstancesAssert(
    res0: Int,
    res1: Int,
    res2: Boolean,
    res3: Boolean
  ): Unit = {

    val intAddition: Monoid[Int] = new Monoid[Int] {
      def op(x: Int, y: Int): Int = x + y
      def zero: Int = res0
    }

    val intMultipication: Monoid[Int] = new Monoid[Int] {
      def op(x: Int, y: Int): Int = x * y
      def zero: Int = res1
    }

    val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
      def op(x: Boolean, y: Boolean): Boolean = x || y
      def zero: Boolean = res2
    }

    def booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
      def op(x: Boolean, y: Boolean): Boolean = x && y
      def zero: Boolean = res3
    }

    intAddition.op(intAddition.zero, 5) shouldBe 5
    intAddition.op(5, intAddition.zero) shouldBe 5

    intMultipication.op(intMultipication.zero, 5) shouldBe 5
    intMultipication.op(5, intMultipication.zero) shouldBe 5

    booleanOr.op(booleanOr.zero, true) shouldBe true
    booleanOr.op(true, booleanOr.zero) shouldBe true
    booleanOr.op(booleanOr.zero, false) shouldBe false
    booleanOr.op(false, booleanOr.zero) shouldBe false

    booleanAnd.op(booleanAnd.zero, true) shouldBe true
    booleanAnd.op(true, booleanAnd.zero) shouldBe true
    booleanAnd.op(booleanAnd.zero, false) shouldBe false
    booleanAnd.op(false, booleanAnd.zero) shouldBe false
  }

  /**
    * <b>Exercise 10.2</b>
    *
    * Let's give a Monoid instance for combining Option values
    */
  def optionMonoidAssert(res0: Option[Int], res1: Option[Int]): Unit = {
    def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
      def op(x: Option[A], y: Option[A]): Option[A] = x.orElse(y)
      def zero: Option[A] = None
    }

    optionMonoid[Int].op(Option(2), Option(3)) shouldBe res0
    optionMonoid[Int].op(Option(2), optionMonoid[Int].zero) shouldBe res1
  }

  /**
    * <b>Exercise 10.3</b>
    *
    * Let's write a monoid for endofunctions (= functions having the same argument and return type)
    */
  def endoMonoidAssert(
    res0: Int,
    res1: Int,
    res2: Int,
    res3: Int
  ): Unit = {
    def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
      def op(f: A => A, g: A => A): A => A = x => f(g(x))
      def zero: A => A = x => x
    }

    endoMonoid[Int].op(_ + 1, _ * 2)(10) shouldBe res0
    endoMonoid[Int].op(_ * 2, _ + 1)(10) shouldBe res1
    endoMonoid[Int].op(_ + 1, endoMonoid[Int].zero)(10) shouldBe res2
    endoMonoid[Int].op(endoMonoid[Int].zero, endoMonoid[Int].zero)(10) shouldBe res3
  }
}
