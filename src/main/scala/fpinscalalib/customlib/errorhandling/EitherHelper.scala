/*
 * Copyright 2016-2020 47 Degrees Open Source <https://www.47deg.com>
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

package fpinscalalib.customlib.errorhandling

// The following implementation of the Either type is provided by Manning as a solution to the multiple implementation
// exercises found in the "Functional Programming in Scala" book. We'll use this one (instead of Scala's Either), in our
// sections related to chapter 4: "Handling error without exceptions". The original code can be found in the following
// URL:
//
// https://github.com/fpinscala/fpinscala/blob/d9476b3323e843d234454ac411a61d353809deee/answers/src/main/scala/fpinscala/errorhandling/Either.scala

import scala.{Option => _, Left => _, Right => _, Either => _, _}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => Right(f(a))
      case Left(e)  => Left(e)
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e)  => Left(e)
      case Right(a) => f(a)
    }
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] =
    this match {
      case Left(_)  => b
      case Right(a) => Right(a)
    }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { a <- this; b1 <- b } yield f(a, b1)
}
case class Left[+E](get: E)  extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil    => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

  def traverse_1[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  /*
  There are a number of variations on `Option` and `Either`. If we want to accumulate multiple errors, a simple
  approach is a new data type that lets us keep a list of errors in the data constructor that represents failures:

  trait Partial[+A,+B]
  case class Errors[+A](get: Seq[A]) extends Partial[A,Nothing]
  case class Success[+B](get: B) extends Partial[Nothing,B]

  There is a type very similar to this called `Validation` in the Scalaz library. You can implement `map`, `map2`,
  `sequence`, and so on for this type in such a way that errors are accumulated when possible (`flatMap` is unable to
  accumulate errors--can you see why?). This idea can even be generalized further--we don't need to accumulate failing
  values into a list; we can accumulate values using any user-supplied binary function.

  It's also possible to use `Either[List[E],_]` directly to accumulate errors, using different implementations of
  helper functions like `map2` and `sequence`.
   */
}
