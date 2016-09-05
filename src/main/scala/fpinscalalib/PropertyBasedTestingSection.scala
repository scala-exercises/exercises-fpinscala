package fpinscalalib

import fpinscalalib.customlib.state.RNG
import org.scalatest.{FlatSpec, Matchers}
import fpinscalalib.customlib.testing.Gen
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
}