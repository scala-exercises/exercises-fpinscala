/*
 *  scala-exercises - exercises-fpinscala
 *  Copyright (C) 2015-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
 */

package fpinscalalib

import fpinscalalib.customlib.state.{RNG, State}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import fpinscalalib.customlib.testing.{Gen, SGen}
import fpinscalalib.customlib.testing.Gen._
import fpinscalalib.customlib.testing.Prop._

/** @param name property_based_testing
 */
object PropertyBasedTestingSection
    extends AnyFlatSpec
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
   * Note: some of the exercises in this chapter are somewhat open-ended, and weren't included in this section. You can
   * always head to the <a href="https://github.com/fpinscala/fpinscala/tree/master/answerkey/testing">official repository</a>
   * containing hints for all exercises available in the book.
   *
   * = A brief tour of property-based testing =
   *
   * <b>Exercise 8.1</b>
   *
   * To get used to property testing, let's try to figure out the properties that specify the implementation of a
   * `sum: List[Int] => Int` function:
   *
   * {{{
   *   * The sum of the empty list is 0.
   *   * The sum of a list whose elements are all equal to `x` is just the list's length multiplied by `x`. We might
   *     express this as `sum(List.fill(n)(x)) == n * x`
   *   * For any list, `l`, `sum(l) == sum(l.reverse)`, since addition is commutative.
   *   * Given a list, `List(x,y,z,p,q)`, `sum(List(x,y,z,p,q)) == sum(List(x,y)) + sum(List(z,p,q))`, since addition is
   *     associative. More generally, we can partition a list into two subsequences whose sum is equal to the sum of the
   *     overall list.
   *   * The sum of 1,2,3...n is `n*(n+1)/2`.
   * }}}
   *
   * <b>Exercise 8.2</b>
   *
   * Likewise, these would be the properties that specify a function that finds the maximum of a `List[Int]`:
   *
   * {{{
   *   * The max of a single element list is equal to that element.
   *   * The max of a list is greater than or equal to all elements of the list.
   *   * The max of a list is an element of that list.
   *   * The max of the empty list is unspecified and should throw an error or return `None`.
   * }}}
   *
   * = The meaning and API of properties =
   *
   * <b>Exercise 8.3</b>
   *
   * Taking this representation of `Prop`, let's implement `&&`:
   *
   * {{{
   *   trait Prop { def check: Boolean }
   *
   *   def &&(p: Prop): Prop = new Prop {
   *     def check = Prop.this.check && p.check
   *   }
   * }}}
   *
   * Let's try it out while testing a couple of properties for Strings. First, a substring of a String containing its
   * last character should be the same that accessing the character directly. Secondly, calling `startsWith` on a string
   * should yield an affirmative result when compared to the original string.
   */
  def propAndAssert(res0: Int, res1: Int, res2: Boolean): Unit = {
    val genString = Gen.stringN(5)

    val propA = forAll(genString)(s => s.substring(res0) == s.charAt(res1).toString)
    val propB = forAll(genString)(s => s.startsWith(s) == res2)

    // Don't worry about the implementations of run and its types, we'll deal with it later:
    val result = (propA && propB).run(100, 100, RNG.Simple(System.currentTimeMillis))
    result shouldBe Passed
  }

  /**
   * = The meaning and API of generators =
   *
   * <b>Exercise 8.4</b>
   *
   * Let's implement `Gen.choose` using the following representation for `Gen`:
   *
   * {{{
   *   case class Gen[A](sample: State[RNG, A])
   * }}}
   *
   * `choose` should generate integers in the range `start` to `stopExclusive`:
   */
  def genChooseIntAssert(res0: Int, res1: Int, res2: Int, res3: Int): Unit = {
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

    val rng = RNG.Simple(47)
    // We use sample on the Gen instance to generate a State containing a RNG and an Int value. We can then run it and
    // obtain both the random-generated number and a new generator to keep generating more.
    choose(res0, 10).sample.run(rng)._1 should be >= res1
    choose(0, res2).sample.run(rng)._1 should be < res3
  }

  /**
   * <b>Exercise 8.5</b>
   *
   * We can implement other functions for `Gen`. Let's look at them, starting by `unit`, that always generates the same
   * value:
   */
  def genUnitAssert(res0: Int, res1: String): Unit = {
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

    val rng = RNG.Simple(47)
    unit(42).sample.run(rng)._1 shouldBe res0
    unit("foo").sample.run(rng)._1 shouldBe res1
  }

  /**
   * `boolean` generates random `Boolean` values:
   *
   * {{{
   *   val boolean: Gen[Boolean] = Gen(State(RNG.boolean))
   * }}}
   *
   * We can also implement `listOfN`, a function that generates lists of length `n` using the provided generator:
   */
  def genListOfN(res0: Int, res1: List[Int]): Unit = {
    def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
      Gen(State.sequence(List.fill(n)(g.sample)))

    val rng            = RNG.Simple(47)
    val listOfBooleans = listOfN(10, Gen.boolean).sample.run(rng)._1
    listOfBooleans.size shouldBe res0

    listOfN(3, Gen.unit(42)).sample.run(rng)._1 shouldBe res1
  }

  /**
   * = Generators that depend on generated values =
   *
   * <b>Exercise 8.6</b>
   *
   * `flatMap`, lets one generator depend on another. This is its implementation:
   *
   * {{{
   *   def flatMap[B](f: A => Gen[B]): Gen[B] =
   *     Gen(sample.flatMap(a => f(a).sample))
   * }}}
   *
   * We can use `flatMap` to implement a more dynamic version of `listOfN`:
   */
  def genListOfNViaFlatMap(res0: Int, res1: Int, res2: List[Int]): Unit = {
    def listOfN_1[A](size: Gen[Int], g: Gen[A]): Gen[List[A]] =
      size flatMap (n => Gen.listOfN(n, g))

    val rng           = RNG.Simple(47)
    val intGen        = choose(0, 10)
    val generatedList = listOfN_1(intGen, unit(42)).sample.run(rng)._1
    generatedList.size should be >= res0
    generatedList.size should be < res1

    listOfN_1(Gen.unit(1), Gen.unit(42)).sample.run(rng)._1 shouldBe res2
  }

  /**
   * <b>Exercise 8.7</b>
   *
   * Through the use of `flatMap` we can implement `union`, a function to combine two generators of the same type into
   * one, by pulling values from each one with the same likelihood:
   *
   * {{{
   *   def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
   *     boolean.flatMap(b => if (b) g1 else g2)
   * }}}
   *
   * <b>Exercise 8.8</b>
   *
   * Following a similar principle we can implement `weighted`, a version of `union` accepting a weight for each
   * `Gen` and generates values from each one with a probability proportional to its weight:
   *
   * {{{
   *   def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
   *     // The probability we should pull from `g1`.
   *     val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
   *     Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
   *   }
   * }}}
   *
   * <b>Exercise 8.9</b>
   *
   * Let's implement `&&` and `\\` to compose `Prop` values:
   *
   * {{{
   *   def &&(p: Prop) = Prop {
   *     (max,n,rng) => run(max,n,rng) match {
   *       case Passed | Proved => p.run(max, n, rng)
   *       case x => x
   *     }
   *   }
   *
   *   def ||(p: Prop) = Prop {
   *     (max,n,rng) => run(max,n,rng) match {
   *         // In case of failure, run the other prop.
   *         case Falsified(msg, _) => p.tag(msg).run(max,n,rng)
   *         case x => x
   *      }
   *   }
   * }}}
   *
   * Let's try those out:
   */
  def propAndOrAssert(res0: Result): Unit = {
    val genZeroToTen      = Gen.choose(0, 10)
    val genElevenToTwenty = Gen.choose(11, 20)
    val genCombination    = Gen.union(genZeroToTen, genElevenToTwenty)

    val combinedProp = (forAll(genCombination)(_ < 10) ||
      forAll(genCombination)(_ < 20)) &&
      forAll(genCombination)(_ >= 0)

    val result = combinedProp.run(100, 100, RNG.Simple(System.currentTimeMillis))
    result shouldBe res0
  }

  /**
   * = Test case minimization =
   *
   * <b>Exercise 8.10</b>
   *
   * Let's implement helper functions to convert `Gen` into the new `SGen` type:
   *
   * {{{
   *   case class SGen[+A](forSize: Int => Gen[A])
   *
   *   case class Gen[+A](sample: State[RNG,A]) {
   *     // ...
   *     def unsized: SGen[A] = SGen(_ => this)
   *   }
   * }}}
   *
   * <b>Exercise 8.11</b>
   *
   * `SGen` supports many of the same operations as `Gen`. Let's define some convenience functions of `SGen` that just
   * delegate to their counterparts on `Gen`:
   *
   * {{{
   *   case class SGen[+A](g: Int => Gen[A]) {
   *     def apply(n: Int): Gen[A] = g(n)
   *
   *     def map[B](f: A => B): SGen[B] = SGen { g(_) map f }
   *
   *     def flatMap[B](f: A => SGen[B]): SGen[B] = {
   *       val g2: Int => Gen[B] = n => {
   *         g(n) flatMap { f(_).g(n) }
   *       }
   *       SGen(g2)
   *     }
   *
   *     def **[B](s2: SGen[B]): SGen[(A,B)] =
   *       SGen(n => apply(n) ** s2(n))
   *     }
   * }}}
   *
   * <b>Exercise 8.12</b>
   *
   * We can also implement a `listOf` combinator that doesn't need an explicit size. It should return an `SGen`, and its
   * implementation should generate lists of the requested size. Let's try it out:
   */
  def sGenListOfAssert(res0: Int): Unit = {
    def listOf[A](g: Gen[A]): SGen[List[A]] =
      SGen(n => g.listOfN(n))

    val gen  = Gen.unit(42)
    val prop = forAll(listOf(gen))(l => l.forall(_ == res0))
    prop.run(100, 100, RNG.Simple(System.currentTimeMillis)) shouldBe Passed
  }

  /**
   * = Using the library and improving its usability =
   *
   * <b>Exercise 8.13</b>
   *
   * Let's define a `listOf1` function to generate nonempty lists:
   */
  def sGenListOf1(res0: Int): Unit = {
    def listOf1[A](g: Gen[A]): SGen[List[A]] =
      SGen(n => g.listOfN(n max res0))

    val prop = forAll(listOf1(Gen.choose(0, 10)))(l => l.size == 1 && l.forall(_ < 10))
    prop.run(100, 100, RNG.Simple(System.currentTimeMillis))
  }

  /**
   * <b>Exercise 8.14</b>
   *
   * Now let's write a property to verify the behavior of `List.sorted`:
   *
   * {{{
   *   val sortedProp = forAll(listOf(smallInt)) { ns =>
   *     val nss = ns.sorted
   *     // We specify that every sorted list is either empty, has one element,
   *     // or has no two consecutive elements `(a,b)` such that `a` is greater than `b`.
   *     (nss.isEmpty || nss.tail.isEmpty || !nss.zip(nss.tail).exists {
   *       case (a,b) => a > b
   *     })
   *     // Also, the sorted list should have all the elements of the input list,
   *     && !ns.exists(!nss.contains(_))
   *     // and it should have no elements not in the input list.
   *     && !nss.exists(!ns.contains(_))
   *   }
   * }}}
   *
   * = Writing a test suite for parallel computations =
   *
   * <b>Exercise 8.16</b>
   *
   * We can write a generator for Par[Int], building deeply nested parallel computations. Take a look:
   *
   * {{{
   *   // A `Gen[Par[Int]]` generated from a list summation that spawns a new parallel
   *   // computation for each element of the input list summed to produce the final
   *   // result. This is not the most compelling example, but it provides at least some
   *   // variation in structure to use for testing.
   *
   *   val pint2: Gen[Par[Int]] = choose(-100,100).listOfN(choose(0,20)).map(l =>
   *     l.foldLeft(Par.unit(0))((p,i) =>
   *       Par.fork { Par.map2(p, Par.unit(i))(_ + _) }))
   * }}}
   *
   * <b>Exercise 8.18</b>
   *
   * With `pint2` we can express the property about `fork` from chapter 7 (`fork(x) == x`):
   *
   * {{{
   *   val forkProp = Prop.forAllPar(pint2)(i => equal(Par.fork(i), i)) tag "fork"
   * }}}
   *
   * == Testing higher-order functions ==
   *
   * <b>Exercise 8.18</b>
   *
   * Let's show how we can test higher-order functions with `takeWhile` and `dropWhile` from `List`:
   */
  def propTakeWhileDropWhile(res0: Result): Unit = {
    val prop = forAll(listOf(Gen.choose(0, 20)))(l => {
      val index = Gen.choose(0, 20).sample.run(RNG.Simple(47))._1
      l.takeWhile(_ < index) ++ l.dropWhile(_ < index) == l
    })
    prop.run(100, 100, RNG.Simple(System.currentTimeMillis)) shouldBe res0
  }
}
