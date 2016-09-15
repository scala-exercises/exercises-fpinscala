package fpinscalalib

import fpinscalalib.customlib.state.{RNG, State}
import org.scalatest.{FlatSpec, Matchers}
import fpinscalalib.customlib.testing.Gen
import fpinscalalib.customlib.testing.Gen._
import fpinscalalib.customlib.testing.Prop
import fpinscalalib.customlib.testing.Prop._



/** @param name property_based_testing
  */
object PropertyBasedTestingSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {
  /**
    * = A brief tour of property-based testing =
    *
    * As an example, in ScalaCheck, a property-based testing library for Scala, a property looks something like this:
    *
    * {{{
    *   // A generator of lists of integers between 0 and 100.
    *   val intList = Gen.listOf(Gen.choose(0,100))
    *   // A property that specifies the behavior of the List.reverse method.
    *   val prop =
    *     // Check that reversing a list twice gives back the original list.
    *     forAll(intList)(ns => ns.reverse.reverse == ns) &&
    *     // Check that the first element becomes the last element after reversal.
    *     forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
    *   // A property which is obviously false.
    *   val failingProp = forAll(intList)(ns => ns.reverse == ns)
    * }}}
    *
    * And we can check properties like so:
    *
    * {{{
    *   scala> prop.check
    *   + OK, passed 100 tests.
    *
    *   scala> failingProp.check
    *   ! Falsified after 6 passed tests.
    *   > ARG_0: List(0, 1)
    * }}}
    *
    * Here, `intList` is not a `List[Int]`, but a `Gen[List[Int]]`, which is something that knows how to generate test
    * data of type `List[Int]`. We can sample from this generator, and it will produce lists of different lengths, filled
    * with random numbers between 0 and 100.
    *
    * The function `forAll` creates a property by combining a generator of type `Gen[A]` with some predicate of type
    * `A => Boolean`. The property asserts that all values produced by the generator should satisfy the predicate. In
    * this simple example we’ve used `&&` to combine two properties. The resulting property will hold only if neither
    * property can be falsified by any of the generated test cases. Together, the two properties form a partial
    * specification of the correct behavior of the `reverse` method.
    *
    * When we invoke `prop.check`, `ScalaCheck` will randomly generate `List[Int]` values to try to find a case that
    * falsifies the predicates that we’ve supplied. The output indicates that `ScalaCheck` has generated 100 test cases
    * (of type `List[Int]`) and that they all satisfied the predicates. Properties can of course fail — the output of
    * `failingProp.check` indicates that the predicate tested false for some input, which is helpfully printed out to
    * facilitate further testing or debugging.
    *
    * To get used to thinking about testing this way, let's try to come up with properties that specify the
    * implementation of a `sum: List[Int] => Int` function:
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
    * ScalaCheck is just one property-based testing library, and we'll be deriving our own library in this chapter from
    * scratch.
    *
    * = Choosing data types and functions =
    *
    * Let’s get started. What data types should we use for our testing library? What primitives should we define, and
    * what might they mean? What laws should our functions satisfy? As before, we can look at a simple example and
    * “read off” the needed data types and functions, and see what we find. For inspiration, let’s look at the ScalaCheck
    * example we showed earlier:
    *
    * {{{
    *   val intList = Gen.listOf(Gen.choose(0,100)) val prop =
    *     forAll(intList)(ns => ns.reverse.reverse == ns) &&
    *     forAll(intList)(ns => ns.headOption == ns.reverse.lastOption)
    * }}}
    *
    * Without knowing anything about the implementation of `Gen.choose` or `Gen.listOf`, we can guess that whatever data
    * type they return (let’s call it `Gen`, short for generator) must be parametric in some type. That is,
    * `Gen.choose(0, 100)` probably returns a `Gen[Int]`, and `Gen.listOf` is then a function with the signature
    * `Gen[Int] => Gen[List[Int]]`. But since it doesn’t seem like `Gen.listOf` should care about the type of the `Gen`
    * it receives as input (it would be odd to require separate combinators for creating lists of `Int`, `Double`,
    * `String`, and so on), let’s go ahead and make it polymorphic:
    *
    * {{{
    *   def listOf[A](a: Gen[A]): Gen[List[A]]
    * }}}
    *
    * Notice that we’re not specifying the size of the list to generate. For this to be implementable, our generator must
    * therefore either assume or be told the size. Assuming a size seems a bit inflexible - any assumption is unlikely to
    * be appropriate in all contexts. So it seems that generators must be told the size of test cases to generate. We can
    * imagine an API where this is made explicit:
    *
    * {{{
    *   def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]]
    * }}}
    *
    * What about the rest of this example? The `forAll` function looks interesting. We can see that it accepts a
    * `Gen[List[Int]]` and what looks to be a corresponding predicate, `List[Int] => Boolean`. But again, it doesn’t
    * seem like `forAll` should care about the types of the generator and the predicate, as long as they match up. We
    * can express this with the type:
    *
    * {{{
    *   def forAll[A](a: Gen[A])(f: A => Boolean): Prop
    * }}}
    *
    * Here, we’ve simply invented a new type, `Prop` (short for property, following the ScalaCheck naming), for the result
    * of binding a `Gen` to a predicate.
    *
    * = The meaning and API of properties =
    *
    * Let’s discuss what we want our types and functions to mean. First, consider `Prop`. We know there exist functions
    * `forAll` (for creating a property), `&&` (for composing properties), and `check` (for running a property). In
    * ScalaCheck, this `check` method has a side effect of printing to the console. It’s fine to expose this as a
    * convenience function, but it’s not a basis for composition. For instance, we couldn’t implement `&&` for `Prop` if
    * its representation were just the check method:
    *
    * {{{
    *   trait Prop {
    *     def check: Unit
    *     def &&(p: Prop): Prop = ???
    *   }
    * }}}
    *
    * In order to combine `Prop` values using combinators like `&&`, we need `check` (or whatever function “runs”
    * properties) to return some meaningful value. What type should that value have? Well, let’s consider what sort of
    * information we’d like to get out of checking our properties. At a minimum, we need to know whether the property
    * succeeded or failed. This lets us implement `&&`.
    *
    * Assuming the following representation of `Prop`, let's implement `&&` as a method of `Prop`:
    *
    * {{{
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
    * In this representation, `Prop` is nothing more than a non-strict `Boolean`, which can probably be insufficient. If
    * a property fails, we might want to know how many tests succeeded first, and what arguments produced the failure.
    * And if a property succeeds, it would be useful to know how many tests it ran. Let’s try returning an `Either` to
    * indicate success or failure:
    *
    * {{{
    *   object Prop {
    *     // Type aliases like this can help the readability of an API.
    *     type FailedCase = String
    *     type SuccessCount = Int
    *   }
    *   trait Prop { def check: Either[((FailedCase, SuccessCount)), SuccessCount] }
    * }}}
    *
    * Regarding the tupe to be returned in a failure case, we don't really care about the type of the value that caused
    * a property to fail. For values that we're going to show to human beings (we're just going to end up printing those
    * to the screen for inspection by the person running the tests), a `String` is absolutely appropiate.
    *
    * In the case of failure, check returns a `Left((s,n))`, where `s` is some `String` that represents the value that
    * caused the property to fail, and `nv is the number of cases that succeeded before the failure occurred.
    *
    * = The meaning and API of generators =
    *
    * We determined earlier that a `Gen[A]` was something that knows how to generate values of type `A`. What are some
    * ways it could do that? Well, it could randomly generate these values. Look back at the example from chapter 6 —
    * there, we gave an interface for a purely functional random number generator `RNG` and showed how to make it
    * convenient to combine computations that made use of it. We could just make `Gen` a type that wraps a `State`
    * transition over a random number generator:
    *
    * {{{
    *   case class Gen[A](sample: State[RNG, A])
    * }}}
    *
    * Let's implement `Gen.choose` using this representation of `Gen`. It should generate integers in the range `start`
    * to `stopExclusive`:
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
    * We can implement other functions based on this representation of `Gen`. Let's look at them, starting by `unit`,
    * that always generates the same value:
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

    val rng = RNG.Simple(47)
    val listOfBooleans = listOfN(10, Gen.boolean).sample.run(rng)._1
    listOfBooleans.size shouldBe res0

    listOfN(3, Gen.unit(42)).sample.run(rng)._1 shouldBe res1
  }

  /**
    * = Generators that depend on generated values =
    *
    * Suppose we’d like a `Gen[(String,String)]` that generates pairs where the second string contains only characters
    * from the first. Or that we had a `Gen[Int]` that chooses an integer between `0` and `11`, and we’d like to make a
    * `Gen[List[Double]]` that then generates lists of whatever length is chosen. In both of these cases there’s a
    * dependency — we generate a value, and then use that value to determine what generator to use next. For this we need
    * `flatMap`, which lets one generator depend on another.
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

    val rng = RNG.Simple(47)
    val intGen = choose(0, 10)
    val generatedList = listOfN_1(intGen, unit(42)).sample.run(rng)._1
    generatedList.size should be >= res0
    generatedList.size should be < res1

    listOfN_1(Gen.unit(1), Gen.unit(42)).sample.run(rng)._1 shouldBe res2
  }

  /**
    * Through the use of `flatMap` we can implement `union`, a function to combine two generators of the same type into
    * one, by pulling values from each one with equal likelihood:
    *
    * {{{
    *   def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    *     boolean.flatMap(b => if (b) g1 else g2)
    * }}}
    *
    * Following a similar principle we can implement `weighted`, a version of `union` that accepts a weight for each
    * `Gen` and generates values from each `Gen` with probability proportional to its weight:
    *
    * {{{
    *   def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    *     // The probability we should pull from `g1`.
    *     val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    *     Gen(State(RNG.double).flatMap(d => if (d < g1Threshold) g1._1.sample else g2._1.sample))
    *   }
    * }}}
    *
    * = Refining the Prop data type =
    *
    * Now that we know more about our representation of generators, let’s return to our definition of `Prop`. Our `Gen`
    * representation has revealed information about the requirements for `Prop`. Our current definition of `Prop` looks
    * like this:
    *
    * {{{
    *   trait Prop {
    *     def check: Either[(FailedCase, SuccessCount), SuccessCount]
    *   }
    * }}}
    *
    * `Prop` is nothing more than a non-strict `Either`. But it’s missing some information. We have the number of
    * successful test cases in `SuccessCount`, but we haven’t specified how many test cases to examine before we consider
    * the property to have passed the test. We can abstract over this dependency. Moreover, note that when a property
    * passes, the user won't need to know how many tests have been executed. Let's create a new data type to express this:
    *
    * {{{
    *   sealed trait Result {
    *     def isFalsified: Boolean
    *   }
    *
    *   // Indicates that all tests passed:
    *   case object Passed extends Result {
    *     def isFalsified = false
    *   }
    *
    *   // Indicates that one of the test cases falsified the property:
    *   case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    *     def isFalsified = true
    *   }
    * }}}
    *
    * To be able to implement `forAll`, we still need to generate random test cases using our current representation of
    * `Gen`. That means it's going to need an `RNG`. We can go ahead and propagate that dependency to `Prop`:
    *
    * {{{
    *   case class Prop(run: (TestCases,RNG) => Result)
    * }}}
    *
    * We now have enough information to actually implement `forAll`. Here’s a simple implementation.
    *
    * {{{
    *   /* Produce an infinite random stream from a `Gen` and a starting `RNG`.
    *   def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    *     (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
    *       // A stream of pairs (a, i) where a is a random value and i is its index in the stream.
    *       case (a, i) => try {
    *         // When a test fails, record the failed case and its index so we know how many tests succeeded before it.
    *         if (f(a)) Passed else Falsified(a.toString, i)
    *
    *         // If a test case generates an exception, record it in the result.
    *       } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    *     }.find(_.isFalsified).getOrElse(Passed)
    *   }
    *
    *   def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    *     // Generates an infinite stream of A values by repeatedly sampling a generator.
    *     Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
    *
    *   def buildMsg[A](s: A, e: Exception): String =
    *     s"test case: $s\n" +
    *     s"generated an exception: ${e.getMessage}\n" +
    *     s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
    * }}}
    */
}