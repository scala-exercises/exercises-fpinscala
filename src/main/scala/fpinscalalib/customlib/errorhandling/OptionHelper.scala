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

// The following implementation of the Option type is provided by Manning as a solution to the multiple implementation
// exercises found in the "Functional Programming in Scala" book. We'll use this one (instead of Scala's Option), in our
// sections related to chapter 4: "Handling error without exceptions". The original code can be found in the following
// URL:
//
// https://github.com/fpinscala/fpinscala/blob/d9476b3323e843d234454ac411a61d353809deee/answers/src/main/scala/fpinscala/errorhandling/Option.scala

//hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter
import scala.{Either => _, Option => _, Some => _}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None    => None
      case Some(a) => Some(f(a))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None    => default
      case Some(a) => a
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  /*
  Of course, we can also implement `flatMap` with explicit pattern matching.
   */
  def flatMap_1[B](f: A => Option[B]): Option[B] =
    this match {
      case None    => None
      case Some(a) => f(a)
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this map (Some(_)) getOrElse ob

  /*
  Again, we can implement this with explicit pattern matching.
   */
  def orElse_1[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case _    => this
    }

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if f(a) => this
      case _               => None
    }
  /*
  This can also be defined in terms of `flatMap`.
   */
  def filter_1(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None            extends Option[Nothing]

object Option {

  def failingFn(i: Int): Int = {
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
    // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    } catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

  // a bit later in the chapter we'll learn nicer syntax for
  // writing functions like this
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  /*
  Here's an explicit recursive version:
   */
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil    => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  /*
  It can also be implemented using `foldRight` and `map2`. The type annotation on `foldRight` is needed here; otherwise
  Scala wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!). This is an
  unfortunate consequence of Scala using subtyping to encode algebraic data types.
   */
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil    => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
}
