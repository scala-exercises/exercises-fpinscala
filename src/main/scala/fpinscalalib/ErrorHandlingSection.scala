package fpinscalalib

import fpinscalalib.customlib.errorhandling.{Employee, None, Option, Some}
import fpinscalalib.customlib.errorhandling.Option._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Success, Try}

/** @param name handling_error_without_exceptions
  */
object ErrorHandlingSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {
  /**
    * = The Option data type =
    *
    * Exceptions break referential transparency and introduce context dependence. Moreover, they are not type-safe,
    * hiding information about the fact that they may occur, to the developer and the compiler. We're going to explore
    * an alternative to exceptions without these drawbacks, without losing out on the primary benefit of exceptions:
    * they allow us to `consolidate and centralize error-handling logic`. The technique we use is based on an old idea:
    * instead of throwing an exception, we return a value indicating that an exceptional condition has occurred. instead
    * of using error codes, we introduce a new generic type for these “possibly defined values” and use higher-order
    * functions to encapsulate common patterns of handling and propagating errors.
    *
    * We introduce a new type, `Option`. As with the previously explored `List`, this type also exists in the Scala
    * standard library, but we're re-creating it here for pedagogical purposes:
    *
    * {{{
    *   sealed trait Option[+A]
    *   case class Some[+A](get: A) extends Option[A]
    *   case object None extends Option[Nothing]
    * }}}
    *
    * Option has two cases: it can be defined, in which case it will be a `Some`, or it can be undefined, in which case
    * it will be `None`.
    *
    * Let's consider an example to use our new type. We're defining a function `mean` that computes the mean of a list,
    * which is undefined if the list is empty:
    */

  def optionMeanAssert(res0: Option[Double]): Unit = {
    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    mean(Seq(1, 2, 3, 4, 5)) shouldBe Some(3.0)
    mean(Seq.empty) shouldBe res0
  }

  /**
    * `Option` can be thought of like a `List` that can contain at most one element, and many of the `List` functions we
    * saw earlier have analogous functions on `Option`. We're going to look at some of these functions, starting by `map`,
    * that applies a function `f` in the `Option` is not `None`:
    *
    * {{{
    *   def map[B](f: A => B): Option[B] = this match {
    *     case None => None
    *     case Some(a) => Some(f(a))
    *   }
    * }}}
    *
    * We can also implement `flatMap`, which applies a function `f` which may also fail, to the `Option` if not `None`:
    *
    * {{{
    *   def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
    * }}}
    *
    * The function `getOrElse` used above, tries to get the value contained in the Option, but if it's a `None` will
    * return the default value provided by the caller. The `B >: A` in the declaration tells that the `B` type parameter
    * must be a supertype of `A`. Furthermore, `default : => B` indicates that the argument is of type B, but won’t be
    * evaluated until it’s needed by the function.
    *
    * {{{
    *   def getOrElse[B>:A](default: => B): B = this match {
    *     case None => default
    *     case Some(a) => a
    *   }
    * }}}
    *
    * `orElse` returns the original `Option` if not `None`, or returns the provided `Option` as an alternative in that
    * case:
    *
    * {{{
    *   def orElse[B>:A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
    * }}}
    *
    * Finally, we can implement a `filter` function that will turn any `Option` into a `None` if it doesn't satisfy the
    * provided predicate:
    *
    * {{{
    *   def filter(f: A => Boolean): Option[A] = this match {
    *     case Some(a) if f(a) => this
    *     case _ => None
    *   }
    * }}}
    *
    */

  /**
    * These higher-order functions allows us to avoid to explictly pattern match on an `Option`. Let's explore when to
    * use each one, starting with `map`. The `map` function can be used to transform the result inside an `Option`, if
    * it exists. We can think of it as proceeding with a computation on the assumption that an error hasn’t occurred;
    * it’s also a way of deferring the error handling to later code.
    *
    * Let's check this out with an example exercise:
    */

  // FIXME: We need to take a deeper look at what's going on with the test generators, the parameters shouldn't be of
  // type Some[_] instead of Option[_] to work with the test specs.

  def optionMapAssert(res0: (Option[Employee]) => Option[String]): Unit = {
    def lookupByName(name: String): Option[Employee] = name match {
      case "Joe" => Some(Employee("Joe", "Finances", Some("Julie")))
      case "Mary" => Some(Employee("Mary", "IT", None))
      case _ => None
    }

    // We can look for our employees, and try to obtain their departments. We will assume that we won't find any errors,
    // and if it's the case, we don't have to worry as the computation will end there. Try to use `map` on the result of
    // calling `lookupByName` to create a function to obtain the department of each employee. Hint: to access the
    // optional employee, use Scala's underscore notation. i.e.:
    //
    // _.getOrElse(Employee("John", "Doe", None))

    def getDepartment : (Option[Employee]) => Option[String] = res0

    val joe = lookupByName("Joe")
    val mary = lookupByName("Mary")
    val invalidEmployee = lookupByName("Foo")

    getDepartment(joe) shouldBe Some("Finances")
    getDepartment(mary) shouldBe Some("IT")
    getDepartment(invalidEmployee) shouldBe None
  }

  /**
    * By using `flatMap` we can chain operations that also can fail, as in the following example. Try to find out who is
    * managing each employee, if applicable:
    */

  def optionFlatMapAssert(res0: (Option[Employee]) => Option[String]): Unit = {
    def lookupByName(name: String): Option[Employee] = name match {
      case "Joe" => Some(Employee("Joe", "Finances", Some("Julie")))
      case "Mary" => Some(Employee("Mary", "IT", None))
      case _ => None
    }

    def getManager : (Option[Employee]) => Option[String] = res0

    val joe = lookupByName("Joe")
    val mary = lookupByName("Mary")
    val invalidEmployee = lookupByName("Foo")

    getManager(joe) shouldBe Some("Julie")
    getManager(mary) shouldBe None
    getManager(invalidEmployee) shouldBe None
  }

  /**
    * Let's try to implement the `variance` function in terms of `flatMap`. If the mean of a sequence is `m`, the
    * variance is the mean of `math.pow(x - m, 2)` for each element in the sequence:
    *
    * {{{
    *   def variance(xs: Seq[Double]): Option[Double] =
    *     mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
    * }}}
    */

  /**
    * We may find in some situations when we need to combine two `Option` values using a binary function, so that if any
    * of those values is `None`, the result value is too; and otherwise it will be the result of applying the provided
    * function. We'll call this function `map2`, take a look at its implementation:
    *
    * {{{
    *   def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    *     a flatMap (aa => b map (bb => f(aa, bb)))
    * }}}
    *
    * Let's see an example of its use. Let's write a function `parseInsuranceRateQuote` which takes the age and a number
    * of speeding tickets as strings, and attempts to call another function called `insuranceRateQuote` if parsing both
    * values is valid:
    *
    * {{{
    *   def parseInsuranceRateQuote( age: String, numberOfSpeedingTickets: String): Option[Double] = {
    *     val optAge: Option[Int] = Try { age.toInt }
    *     val optTickets: Option[Int] = Try { numberOfSpeedingTickets.toInt } map2(optAge, optTickes)(insuranceRateQuote)
    *   }
    * }}}
    *
    * As `Try` will return an `Option` containing the value of the operation it encapsulates (or a `None` if it returns
    * an exception), to combine both values we need to make use of the new `map2` function we just implemented.
    */

  /**
    * Let's continue looking at a few other similar cases. For instance, the `sequence` function, which combines a list
    * of `Option`s into one `Option` containing a list of all the `Some` values in the original list. If the original
    * list contains `None` even once, the result of the function should be `None`. Otherwise the result should be a `Some`
    * with a list of all the values:
    *
    * {{{
    *   def sequence(a: List[Option[A]]): Option[List[A]] = a match {
    *     case Nil => Some(Nil)
    *     case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    *   }
    * }}}
    *
    * After taking a look at the implementation, check how it works in the following exercise:
    */

  def optionSequenceAssert(res0: Some[List[Int]], res1: Option[List[Int]]): Unit = {
    sequence(List(Some(1), Some(2), Some(3))) shouldBe res0
    sequence(List(Some(1), Some(2), None)) shouldBe res1
  }

  /**
    * The last `Option` function we're going to explore is `traverse`, that will allow us to map over a list using a
    * function that might fail, returning `None` if applying it to any element of the list returns `None`. Note that we
    * want to avoid traversing the list twice (first to apply the provided function to each element, and another to
    * combine these `Option` values into an optional `List`:
    *
    * {{{
    *   def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    *     case Nil => Some(Nil)
    *     case h::t => map2(f(h), traverse(t)(f))(_ :: _)
    *   }
    * }}}
    *
    * We can also implement `traverse` in terms of `foldRight`:
    *
    * {{{
    *   def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    *     a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))
    * }}}
    *
    * We can even re-implement `sequence` in terms of `traverse`:
    *
    * {{{
    *   def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
    * }}}
    *
    * Let's try `traverse` out, by trying to parse a `List[String]` into a `Option[List[Int]]`:
    */

  def optionTraverseAssert(res0: Some[List[Int]], res1: Option[List[Int]]): Unit = {
    val list1 = List("1", "2", "3")
    val list2 = List("I", "II", "III", "IV")

    def parseInt(a: String): Option[Int] = Try(a.toInt) match {
      case Success(r) => Some(r)
      case _ => None
    }

    traverse(list1)(i => parseInt(i)) shouldBe res0
    traverse(list2)(i => parseInt(i)) shouldBe res1
  }
}
