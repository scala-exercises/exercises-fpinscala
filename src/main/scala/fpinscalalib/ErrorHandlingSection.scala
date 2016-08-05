package fpinscalalib

import fpinscalalib.customlib.errorhandling._
import fpinscalalib.customlib.errorhandling.Option._
import fpinscalalib.customlib.errorhandling.ExampleHelper._
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

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
    * Let's try it out:
    */

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

    getDepartment(lookupByName("Joe")) shouldBe Some("Finances")
    getDepartment(lookupByName("Mary")) shouldBe Some("IT")
    getDepartment(lookupByName("Foo")) shouldBe None
  }

  /**
    * We can also implement `flatMap`, which applies a function `f` which may also fail, to the `Option` if not `None`:
    *
    * {{{
    *   def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
    * }}}
    *
    * By using `flatMap` we can chain operations that also can fail, as in the following example. Try to find out who is
    * managing each employee, if applicable:
    */

  def optionFlatMapAssert(res0: (Option[Employee]) => Option[String]): Unit = {
    def getManager : (Option[Employee]) => Option[String] = res0

    getManager(lookupByName("Joe")) shouldBe Some("Julie")
    getManager(lookupByName("Mary")) shouldBe None
    getManager(lookupByName("Foo")) shouldBe None
  }

  /**
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
    */

  def optionOrElseAssert(res0: Some[Int], res1: Some[Int]): Unit = {
    Try("1".toInt).orElse(Some(0)) shouldBe res0
    Try("IV".toInt).orElse(Some(0)) shouldBe res1
  }

   /** Finally, we can implement a `filter` function that will turn any `Option` into a `None` if it doesn't satisfy the
    * provided predicate:
    *
    * {{{
    *   def filter(f: A => Boolean): Option[A] = this match {
    *     case Some(a) if f(a) => this
    *     case _ => None
    *   }
    * }}}
    *
    * Try it out to discard those employees who belong to the IT department:
    */

  def optionFilterAssert(res0: Some[Int], res1: Option[Int]): Unit = {
     Try("1".toInt).filter(_ % 2 != 0) shouldBe res0
     Try("10".toInt).filter(_ % 2 != 0) shouldBe res1
  }

  /**
    * Let's implement the `variance` function in terms of `flatMap`. If the mean of a sequence is `m`, the variance
    * is the mean of `math.pow(x - m, 2)` for each element in the sequence:
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
    *   def Try[A](a: => A): Option[A] =
    *     try Some(a)
    *     catch { case e: Exception => None }
    *
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

    traverse(list1)(i => Try(i.toInt)) shouldBe res0
    traverse(list2)(i => Try(i.toInt)) shouldBe res1
  }

  /**
    * = The Either data type =
    *
    * One thing you may have noticed with `Option` is that it doesn’t tell us anything about what went wrong in the case
    * of an exceptional condition. All it can do is give us `None`, indicating that there’s no value to be had. But
    * sometimes we want to know more. For example, we might want a `String` that gives more information, or if an
    * exception was raised, we might want to know what that error actually was.
    *
    * In this section, we’ll walk through a simple extension to `Option`, the `Either` data type, which lets us track a
    * reason for the failure. Let’s look at its definition:
    *
    * {{{
    *   sealed trait Either[+E, +A]
    *   case class Left[+E](value: E) extends Either[E, Nothing]
    *   case class Right[+A](value: A) extends Either[Nothing, A]
    * }}}
    *
    * `Either` has only two cases, just like `Option`. The essential difference is that both cases carry a value. When
    * we use it to indicate success or failure, by convention the `Right` constructor is reserved for the success case
    * (a pun on “right,” meaning correct), and `Left` is used for failure.
    *
    * Let's look at the `mean` example again, this time returning a `String` in case of failure:
    */

  def eitherMeanAssert(res0: Right[Double], res1: Left[String]): Unit = {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty)
        Left("mean of empty list!")
      else
        Right(xs.sum / xs.length)

    mean(IndexedSeq(1.0, 2.0, 3.0, 4.0, 5.0)) shouldBe res0
    mean(IndexedSeq.empty) shouldBe res1
  }

  /**
    * As we did with `Option`, let's implement versions of `map`, `flatMap`, `orElse` and `map2` on `Either` that
    * operate on the `Right` value, starting with `map`:
    *
    * {{{
    *   def map[B](f: A => B): Either[E, B] = this match {
    *     case Right(a) => Right(f(a))
    *     case Left(e) => Left(e)
    *   }
    * }}}
    *
    * In the same fashion as `Option`, `map` allow us to chain operations on an `Either` without worrying about the
    * possible errors that may arise, as the chain will stop if any error occurs. Let's try it out, by improving the
    * employee lookup function we implemented before, to use `Either` instead of `Option`. Try to use `map` on the
    * `Either` type to obtain the department of each employee:
    */

  def eitherMapAssert(res0: (Either[String, Employee]) => Either[String, String]): Unit = {
    def lookupByNameViaEither(name: String): Either[String, Employee] = name match {
      case "Joe" => Right(Employee("Joe", "Finances", Some("Julie")))
      case "Mary" => Right(Employee("Mary", "IT", None))
      case _ => Left("Employee not found")
    }

    def getDepartment : (Either[String, Employee]) => Either[String, String] = res0

    getDepartment(lookupByNameViaEither("Joe")) shouldBe Right("Finances")
    getDepartment(lookupByNameViaEither("Mary")) shouldBe Right("IT")
    getDepartment(lookupByNameViaEither("Foo")) shouldBe Left("Employee not found")
  }

  /**
    * `flatMap` behaves the same in `Either` as in `Option`, allowing us to chain operations that may also fail. To test
    * this, we'll rewrite the previously seen `Try` function, to return `Either`:
    *
    * {{{
    *   def TryEither[A](a: => A): Either[String, A] =
    *     try Right(a)
    *     catch { case e: Exception => Left(e.getMessage) }
    * }}}
    *
    * In this re-implementation, `TryEither` will return a positive `Right` result when the provided value is a correct
    * one, or a `Left` containing the exception message if there was an error. Let's try it out. (Hint: the exception
    * message for a division by zero will be "/ by zero").
    */

  def eitherFlatMapAssert(res0: Right[Int], res1: Left[String]): Unit = {
    TryEither("10".toInt).flatMap(i => TryEither(i / 2)) shouldBe res0
    TryEither("0".toInt).flatMap(i => TryEither(i / 0)) shouldBe res1

  }

  /**
    * `orElse` works the same as in `Option`s, returning the original `Either` when it contains a `Right`, or the
    * provided alternative in case it's a `Left`:
    *
    * {{{
    *   def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = this match {
    *     case Left(_) => b
    *     case Right(a) => Right(a)
    *   }
    * }}}
    *
    * To check how it behaves, let's use `orElse` to create our own error messages for certain operations, instead of
    * relying on the default ones:
    */

  def eitherOrElseAssert(res0: Right[Int], res1: Left[String], res2: Right[Int], res3: Left[String]): Unit = {
    TryEither("IV".toInt).orElse(Left("Parsing error")) shouldBe res1
    TryEither(10 / 0).orElse(Left("Division by zero error")) shouldBe res3
  }

  /**
    * In the same fashion as with `Option`s, `map2` lets us combine two `Either`s using a binary function. Note that we
    * will use for-comprehensions instead of a chain of `flatMap` and `map` calls:
    *
    * {{{
    *   def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):  Either[EE, C] =
    *   for {
    *     a <- this;
    *     b1 <- b
    *   } yield f(a,b1)
    * }}}
    *
    * In this implementation, we can't report errors on both sides. To do that, we would need a new data type that could
    * hold a list of errors:
    *
    * {{{
    *   trait Partial[+A,+B]
    *   case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
    *   case class Success[+B](get: B) extends Partial[Nothing,B]
    * }}}
    *
    * This data type is really similar to Scalaz' `Validation` type.
    *
    * In any case, let's test `map2` on the following exercise, to find out if two employees share a department by using
    * an specific function:
    */

  def eitherMap2Assert(res0: Right[Boolean], res1: Right[Boolean], res2: Left[String]): Unit = {
    def employeesShareDepartment(employeeA: Employee, employeeB: Employee) = employeeA.department == employeeB.department

    lookupByNameViaEither("Joe").map2(lookupByNameViaEither("Mary"))(employeesShareDepartment) shouldBe res0
    lookupByNameViaEither("Mary").map2(lookupByNameViaEither("Izumi"))(employeesShareDepartment) shouldBe res1
    lookupByNameViaEither("Foo").map2(lookupByNameViaEither("Izumi"))(employeesShareDepartment) shouldBe res2
  }

  /**
    * `sequence` and `traverse` can also be implemented for `Either`. These should return the first error that's
    * encountered, if there is one.
    *
    * {{{
    *   def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    *     case Nil => Right(Nil)
    *     case h::t => (f(h) map2 traverse(t)(f))(_ :: _)
    *   }
    * }}}
    *
    * {{{
    *   def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(x => x)
    * }}}
    *
    * We can try to lookup through a list of employee names to obtain a list of `Employees`:
    */

  def eitherTraverseAssert(res0: Right[List[Employee]], res1: Left[String]): Unit = {
    val employees = List("Joe", "Mary")
    val employeesAndOutsources = employees :+ "Foo"

    Either.traverse(employees)(lookupByNameViaEither) shouldBe res0
    Either.traverse(employeesAndOutsources)(lookupByNameViaEither) shouldBe res1
  }

  /**
    * As for `sequence`, we can create a `List` of employees we looked up by using the `lookupByNameViaEither`, and
    * find out if we were looking for some missing person:
    */

  def eitherSequenceAssert(res0: Right[List[Employee]], res1: Left[String]): Unit = {
    val employees = List(lookupByNameViaEither("Joe"), lookupByNameViaEither("Mary"))
    val employeesAndOutsources = employees :+ lookupByNameViaEither("Foo")

    Either.sequence(employees) shouldBe res0
    Either.sequence(employeesAndOutsources) shouldBe res1
  }
}
