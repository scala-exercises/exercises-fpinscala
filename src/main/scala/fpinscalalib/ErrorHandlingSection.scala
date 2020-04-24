/*
 * Copyright 2016-2020 47 Degrees <https://47deg.com>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package fpinscalalib

import fpinscalalib.customlib.errorhandling._
import fpinscalalib.customlib.errorhandling.Option._
import fpinscalalib.customlib.errorhandling.ExampleHelper._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Success, Try}

/** @param name handling_error_without_exceptions
 */
object ErrorHandlingSection
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
   * = The Option data type =
   *
   * <b>Exercise 4.1</b>:
   *
   * We're going to look at some of the functions available in the `Option`, starting by `map`, that applies a function
   * `f` in the `Option` is not `None`:
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
      case "Joe"   => Some(Employee("Joe", "Finances", Some("Julie")))
      case "Mary"  => Some(Employee("Mary", "IT", None))
      case "Izumi" => Some(Employee("Izumi", "IT", Some("Mary")))
      case _       => None
    }

    /*
     We can look for our employees, and try to obtain their departments. We will assume that we won't find any errors,
     and if it's the case, we don't have to worry as the computation will end there. Try to use `map` on the result of
     calling `lookupByName` to create a function to obtain the department of each employee. Hint: to access the
     optional employee, use Scala's underscore notation. i.e.:

     _.getOrElse(Employee("John", "Doe", None))

     Employee is defined as:

     case class Employee(name: String, department: String, manager: Option[String])
     */
    def getDepartment: (Option[Employee]) => Option[String] = res0

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
   * Try to find out who is managing each employee, if applicable:
   */
  def optionFlatMapAssert(res0: (Option[Employee]) => Option[String]): Unit = {
    def getManager: (Option[Employee]) => Option[String] = res0

    getManager(lookupByName("Joe")) shouldBe Some("Julie")
    getManager(lookupByName("Mary")) shouldBe None
    getManager(lookupByName("Foo")) shouldBe None
  }

  /**
   * The function `getOrElse` tries to get the value contained in the Option, but if it's a `None`, it will
   * return the default value provided by the caller:
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
   * Check how it works in the following exercise:
   */
  def optionOrElseAssert(res0: Some[String], res1: Some[String], res2: Some[String]): Unit = {
    def getManager(employee: Option[Employee]): Option[String] = employee.flatMap(_.manager)

    getManager(lookupByName("Joe")).orElse(Some("Mr. CEO")) shouldBe res0
    getManager(lookupByName("Mary")).orElse(Some("Mr. CEO")) shouldBe res1
    getManager(lookupByName("Foo")).orElse(Some("Mr. CEO")) shouldBe res2
  }

  /**
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
   * Test it out by discarding those employees who belong to the IT department:
   */
  def optionFilterAssert(
      res0: Some[Employee],
      res1: Option[Employee],
      res2: Option[Employee]
  ): Unit = {
    lookupByName("Joe").filter(_.department != "IT") shouldBe res0
    lookupByName("Mary").filter(_.department != "IT") shouldBe res1
    lookupByName("Foo").filter(_.department != "IT") shouldBe res2
  }

  /**
   * <b>Exercise 4.2:</b>
   *
   * Let's implement the `variance` function in terms of `flatMap`. If the mean of a sequence is `m`, the variance
   * is the mean of `math.pow(x - m, 2)` for each element in the sequence:
   *
   * {{{
   *   def variance(xs: Seq[Double]): Option[Double] =
   *     mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
   * }}}
   *
   *
   * <b>Exercise 4.3:</b>
   *
   * Let's write a generic function to combine two `Option` values , so that if any of those values is `None`, the
   * result value is too; and otherwise it will be the result of applying the provided function:
   *
   * {{{
   *   def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
   *     a flatMap (aa => b map (bb => f(aa, bb)))
   * }}}
   *
   * <b>Exercise 4.4:</b>
   *
   * Let's continue by looking at a few other similar cases. For instance, the `sequence` function, which combines a list
   * of `Option`s into another `Option` containing a list of all the `Some`s in the original one. If the original
   * list contains `None` at least once, the result of the function should be `None`. If not, the result should be a
   * `Some` with a list of all the values:
   *
   * {{{
   *   def sequence(a: List[Option[A]]): Option[List[A]] = a match {
   *     case Nil => Some(Nil)
   *     case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
   *   }
   * }}}
   *
   * After taking a look at the implementation, see how it works in the following exercise:
   */
  def optionSequenceAssert(res0: Some[List[Int]], res1: Option[List[Int]]): Unit = {
    sequence(List(Some(1), Some(2), Some(3))) shouldBe res0
    sequence(List(Some(1), Some(2), None)) shouldBe res1
  }

  /**
   * <b>Exercise 4.5:</b>
   *
   * The last `Option` function we're going to explore is `traverse`, that will allow us to map over a list using a
   * function that might fail, returning `None` if applying it to any element of the list returns `None`:
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
      case _          => None
    }

    traverse(list1)(i => parseInt(i)) shouldBe res0
    traverse(list2)(i => parseInt(i)) shouldBe res1
  }

  /**
   * = The Either data type =
   *
   * <b>Exercise 4.6:</b>
   *
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
   * In the same fashion as `Option`, `map` allows us to chain operations on an `Either` without worrying about the
   * possible errors that may arise, as the chain will stop if any error occurs. Let's try it out, by improving the
   * employee lookup function we implemented before, to use `Either` instead of `Option`. Try to use `map` on the
   * `Either` type to obtain the department of each employee:
   */
  def eitherMapAssert(res0: (Either[String, Employee]) => Either[String, String]): Unit = {
    def lookupByNameViaEither(name: String): Either[String, Employee] = name match {
      case "Joe"   => Right(Employee("Joe", "Finances", Some("Julie")))
      case "Mary"  => Right(Employee("Mary", "IT", None))
      case "Izumi" => Right(Employee("Izumi", "IT", Some("Mary")))
      case _       => Left("Employee not found")
    }

    def getDepartment: (Either[String, Employee]) => Either[String, String] = res0

    getDepartment(lookupByNameViaEither("Joe")) shouldBe Right("Finances")
    getDepartment(lookupByNameViaEither("Mary")) shouldBe Right("IT")
    getDepartment(lookupByNameViaEither("Foo")) shouldBe Left("Employee not found")
  }

  /**
   * `flatMap` behaves the same in `Either` as it does in `Option`, allowing us to chain operations that may also fail.
   * Use it to try to obtain the managers from each employee. Note that when calling our `getManager` function, we can
   * find two different errors in its execution:
   */
  def eitherFlatMapAssert(res0: Right[String], res1: Left[String], res2: Left[String]): Unit = {
    def getManager(employee: Either[String, Employee]): Either[String, String] =
      employee.flatMap(e =>
        e.manager match {
          case Some(e) => Right(e)
          case _       => Left("Manager not found")
        }
      )

    getManager(lookupByNameViaEither("Joe")) shouldBe res0
    getManager(lookupByNameViaEither("Mary")) shouldBe res1
    getManager(lookupByNameViaEither("Foo")) shouldBe res2
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
   * Let's check out how it behaves. Let's assume that everyone inside our company ends up responding to a "Mr. CEO"
   * manager. We can provide that logic with `orElse`:
   */
  def eitherOrElseAssert(res0: Right[String], res1: Right[String], res2: Right[String]): Unit = {
    def getManager(employee: Either[String, Employee]): Either[String, String] =
      employee.flatMap(e =>
        e.manager match {
          case Some(e) => Right(e)
          case _       => Left("Manager not found")
        }
      )

    getManager(lookupByNameViaEither("Joe")).orElse(Right("Mr. CEO")) shouldBe res0
    getManager(lookupByNameViaEither("Mary")).orElse(Right("Mr. CEO")) shouldBe res1
    getManager(lookupByNameViaEither("Foo")).orElse(Right("Mr. CEO")) shouldBe res2
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
   * In this implementation, we can't report errors on both sides. To do that, we would need a new data type that can
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
    def employeesShareDepartment(employeeA: Employee, employeeB: Employee) =
      employeeA.department == employeeB.department

    lookupByNameViaEither("Joe").map2(lookupByNameViaEither("Mary"))(employeesShareDepartment) shouldBe res0
    lookupByNameViaEither("Mary").map2(lookupByNameViaEither("Izumi"))(employeesShareDepartment) shouldBe res1
    lookupByNameViaEither("Foo").map2(lookupByNameViaEither("Izumi"))(employeesShareDepartment) shouldBe res2
  }

  /**
   * <b>Exercise 4.7:</b>
   *
   * `sequence` and `traverse` can also be implemented for `Either`. Those functions should return the first error that
   * can be found, if there is one.
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
   * We can attempt to obtain a record of employees names by looking up a list of `Employees`:
   */
  def eitherTraverseAssert(res0: Right[List[Employee]], res1: Left[String]): Unit = {
    val employees              = List("Joe", "Mary")
    val employeesAndOutsources = employees :+ "Foo"

    Either.traverse(employees)(lookupByNameViaEither) shouldBe res0
    Either.traverse(employeesAndOutsources)(lookupByNameViaEither) shouldBe res1
  }

  /**
   * As for `sequence`, we can create a `List` of the employees we looked up by using the `lookupByNameViaEither`, and
   * find out if we were looking for a missing person:
   */
  def eitherSequenceAssert(res0: Right[List[Employee]], res1: Left[String]): Unit = {
    val employees              = List(lookupByNameViaEither("Joe"), lookupByNameViaEither("Mary"))
    val employeesAndOutsources = employees :+ lookupByNameViaEither("Foo")

    Either.sequence(employees) shouldBe res0
    Either.sequence(employeesAndOutsources) shouldBe res1
  }
}
