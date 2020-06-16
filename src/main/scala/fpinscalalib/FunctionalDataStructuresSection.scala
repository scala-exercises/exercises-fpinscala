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

package fpinscalalib

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import fpinscalalib.customlib.functionaldatastructures._
import fpinscalalib.customlib.functionaldatastructures.List._
import Tree._

/** @param name functional_data_structures
 */
object FunctionalDataStructuresSection
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
   * = Singly linked lists =
   *
   * Assume the following functions are available for your reference:
   *{{{
   *    sealed trait List[+A]
   *    case object Nil extends List[Nothing]
   *    case class Cons[+A](head: A, tail: List[A]) extends List[A]
   *}}}
   * <b>Exercise 3.1:</b>
   *
   * Examine the next complex match expression. What will be the result?
   */
  def complexPatternAssert(res0: Int): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }
    x shouldBe res0
  }

  /**
   * <b>Exercise 3.2:</b>
   *
   * Take a look at the implementation of `List`'s `tail` function, and check its behaviour on the following cases:
   */
  def listTakeAssert(res0: List[Int], res1: List[Int]) = {
    def tail[A](l: List[A]): List[A] =
      l match {
        case Nil        => sys.error("tail of empty list")
        case Cons(_, t) => t
      }
    tail(List(1, 2, 3)) shouldBe res0
    tail(List(1)) shouldBe res1
  }

  /**
   * <b>Exercise 3.3:</b>
   *
   * `setHead` follows a similar principle. Let's take a look at how it works:
   */
  def listSetHeadAssert(res0: List[Int], res1: List[String]) = {
    def setHead[A](l: List[A], h: A): List[A] =
      l match {
        case Nil        => sys.error("setHead on empty list")
        case Cons(_, t) => Cons(h, t)
      }
    setHead(List(1, 2, 3), 3) shouldBe res0
    setHead(List("a", "b"), "c") shouldBe res1
  }

  /**
   * <b>Exercise 3.4:</b>
   *
   * We can generalize `take` to the function `drop`:
   */
  def listDropAssert(
      res0: List[Int],
      res1: List[Int],
      res2: List[Int],
      res3: List[Int],
      res4: List[Int]
  ) = {
    def drop[A](l: List[A], n: Int): List[A] =
      if (n <= 0) l
      else
        l match {
          case Nil        => Nil
          case Cons(_, t) => drop(t, n - 1)
        }

    drop(List(1, 2, 3), 1) shouldBe res0
    drop(List(1, 2, 3), 0) shouldBe res1
    drop(List("a", "b"), 2) shouldBe res2
    drop(List(1, 2), 3) shouldBe res3
    drop(Nil, 1) shouldBe res4
  }

  /**
   * <b>Exercise 3.5:</b>
   *
   * `dropWhile` extends the behaviour of `drop`, removing elements from the `List` prefix as long as they match a
   * predicate. Study its implementation and check how it works with the following examples:
   */
  def listDropWhileAssert(res0: List[Int], res1: List[Int], res2: List[Int], res3: List[Int]) = {
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
      l match {
        case Cons(h, t) if f(h) => dropWhile(t, f)
        case _                  => l
      }

    dropWhile(List(1, 2, 3), (x: Int) => x < 2) shouldBe res0
    dropWhile(List(1, 2, 3), (x: Int) => x > 2) shouldBe res1
    dropWhile(List(1, 2, 3), (x: Int) => x > 0) shouldBe res2
    dropWhile(Nil, (x: Int) => x > 0) shouldBe res3
  }

  /**
   * <b>Exercise 3.6:</b>
   *
   * `init` can be implemented in the same fashion, but cannot be implemented in constant time like `tail`:
   */
  def listInitAssert(res0: List[Int], res1: List[Int]) = {
    def init[A](l: List[A]): List[A] =
      l match {
        case Nil          => sys.error("init of empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t)   => Cons(h, init(t))
      }
    init(List(1, 2, 3)) shouldBe res0
    init(List(1)) shouldBe res1
  }

  /**
   * = Recursion over lists and generalizing to higher-order functions =
   *
   * <b>Exercise 3.x:</b>
   *
   * Let's run through the steps that `foldRight` will follow in our new implementation of `sum2`:
   *
   */
  def listFoldRightSumAssert(
      res0: Int,
      res1: Int,
      res2: Int,
      res3: Int,
      res4: Int,
      res5: Int,
      res6: List[Int],
      res7: Int,
      res8: Int,
      res9: Int,
      res10: Int
  ) = {
    foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x, y) => x + y) shouldBe 6
    res0 + foldRight(Cons(2, Cons(3, Nil)), 0)((x, y) => x + y) shouldBe 6
    res1 + res2 + foldRight(Cons(3, Nil), 0)((x, y) => x + y) shouldBe 6
    res3 + res4 + res5 + foldRight(res6, 0)((x, y) => x + y) shouldBe 6
    res7 + res8 + res9 + res10 shouldBe 6
  }

  /**
   * <b>Exercise 3.8:</b>
   *
   * Now that we know how `foldRight` works, try to think about what happens when you pass `Nil` and `Cons` themselves
   * to `foldRight`.
   */
  def listFoldRightNilConsAssert(res0: List[Int]) =
    foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) shouldBe res0

  /**
   * <b>Exercise 3.9:</b>
   *
   * Let's try to use `foldRight` to calculate the length of a list. Try to fill the gaps in our implementation:
   */
  def listLengthAssert(res0: Int, res1: Int): Unit = {
    def l                           = List(1, 2, 3, 4, 5)
    def length[A](as: List[A]): Int = List.foldRight(as, res0)((_, acc) => acc + res1)

    length(l) shouldBe 5
  }

  /**
   * <b>Exercise 3.10:</b>
   *
   * Let's write another general tail-recursive list-recursion function, `foldLeft`, using the techniques we discussed
   * in the previous chapter:
   *
   * {{{
   *   def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
   *     l match {
   *       case Nil => z
   *       case Cons(h,t) => foldLeft(t, f(z,h))(f)
   *     }
   * }}}
   *
   * <b>Exercise 3.11:</b>
   *
   * Let's write functions `sum`, `product` and `length` of a list using `foldLeft`:
   */
  def listFoldLeftSumProductLengthAssert(res0: Int, res1: Double, res2: Int, res3: Int): Unit = {
    def sum3(l: List[Int])          = foldLeft(l, res0)(_ + _)
    def product3(l: List[Double])   = foldLeft(l, res1)(_ * _)
    def length2[A](l: List[A]): Int = foldLeft(l, res2)((acc, h) => acc + res3)

    def listInts    = List(1, 2, 3, 4, 5)
    def listDoubles = List(1.0, 2.0, 3.0)
    sum3(listInts) shouldBe 15
    product3(listDoubles) shouldBe 6.0
    length2(listInts) shouldBe 5
  }

  /**
   * <b>Exercise 3.12:</b>
   *
   * As we saw above, we can write the previous functions we implemented using `foldRight` with `foldLeft`. Let's continue
   * with `reverse`:
   *
   * {{{
   *   def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
   * }}}
   *
   * <b>Exercise 3.13:</b>
   *
   * In fact, we can write `foldLeft` in terms of `foldRight`, and the other way around:
   *
   * {{{
   *   def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A,B) => B): B =
   *     foldLeft(reverse(l), z)((b,a) => f(a,b))
   *
   *   def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
   *     foldRight(l, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
   * }}}
   *
   * <b>Exercise 3.14:</b>
   *
   * Another function we can implement by using `foldRight` is `append`:
   *
   * {{{
   *   def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
   *     foldRight(l, r)(Cons(_,_))
   * }}}
   *
   * Take a look at its implementation and check how it works:
   */
  def listAppendAssert(res0: List[Int], res1: List[Int], res2: List[Int], res3: List[Int]): Unit = {
    append(List(1, 2, 3), List(1, 2)) shouldBe res0
    append(List(1, 2, 3), Nil) shouldBe res1
    append(Nil, List(1, 2)) shouldBe res2
    append(Nil, Nil) shouldBe res3
  }

  /**
   * <b>Exercise 3.15:</b>
   *
   * `foldRight` can also be useful to write a function `concat` that concatenates a list of lists into a single list.
   * Take a look at its implementation:
   *
   * {{{
   *   def concat[A](l: List[List[A]]): List[A] =
   *     foldRight(l, Nil:List[A])(append)
   * }}}
   *
   * Since `append` takes time proportional to its first argument, and this first argument never grows because of the
   * right-associativity of `foldRight`, this function is linear in the total length of all lists.
   *
   * = More functions for working with lists =
   *
   * <b>Exercise 3.16:</b>
   *
   * Let's keep digging into the uses of `foldLeft` and `foldRight`, by implementing a function that transforms a list
   * of integers by adding 1 to each element:
   */
  def listAdd1Assert(res0: Int): Unit = {
    def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => Cons(h + res0, t))
    add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  /**
   * <b>Exercise 3.17:</b>
   *
   * We can do something similar to turn each value in a List[Double] into a String:
   *
   * {{{
   *   def doubleToString(l: List[Double]): List[String] =
   *     foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))
   * }}}
   *
   * <b>Exercise 3.18:</b>
   *
   * Both `add1` and `doubleToString` modify each element in a list while maintaining its structure. We can generalize
   * it in the following way:
   *
   * {{{
   *   def map[A,B](l: List[A])(f: A => B): List[B] =
   *     foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
   * }}}
   *
   * <b>Exercise 3.19:</b>
   *
   * Let's apply the same principle as we use in `map` to remove elements from a list, starting with a function to
   * remove all odd numbers from a List[Int]:
   */
  def listRemoveOdds(res0: Int, res1: Int): Unit = {
    def removeOdds(l: List[Int]): List[Int] =
      foldRight(l, Nil: List[Int])((h, t) => if (h % res0 == res1) Cons(h, t) else t)
    removeOdds(List(1, 2, 3, 4, 5)) shouldBe List(2, 4)
  }

  /**
   * <b>Exercise 3.20:</b>
   *
   * We're going to implement a new function that works like `map` except that the function given will return a list
   * instead of a single result, and that list should be inserted into the final resulting list:
   *
   * {{{
   *   def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
   *     concat(map(l)(f))
   * }}}
   *
   * Let's try it out:
   */
  def listFlatMapAssert(res0: List[Int]): Unit =
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe res0

  /**
   * <b>Exercise 3.21:</b>
   *
   * We can also implement `filter` using `flatMap`:
   *
   * {{{
   *   def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
   *     flatMap(l)(a => if (f(a)) List(a) else Nil)
   * }}}
   *
   */
  /**
   * <b>Exercise 3.22:</b>
   *
   * Now we're going to write a function that accepts two lists of integers and constructs a new list by adding
   * corresponding elements. i.e.: `List(1, 2, 3)` and `List(4, 5, 6)` become `List(5, 7, 9)`:
   *
   * {{{
   *   def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
   *     case (Nil, _) => Nil
   *     case (_, Nil) => Nil
   *     case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
   *   }
   * }}}
   *
   * <b>Exercise 3.23:</b>
   *
   * We can generalize the function above so that it's not specific to integers or addition, `zipWith`:
   *
   * {{{
   *   def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
   *     case (Nil, _) => Nil
   *     case (_, Nil) => Nil
   *     case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
   *   }
   * }}}
   *
   * Let's try out `zipWith` in the following exercise:
   */
  def listZipWithAssert(res0: List[String], res1: List[String]): Unit = {
    zipWith(List("a", "b", "c"), List("A", "B", "C"))(_ + _) shouldBe res0
    zipWith(List(1, 2, 3), List(4, 5, 6))(_.toString + _.toString()) shouldBe res1
  }

  /**
   * <b>Exercise 3.24:</b>
   *
   * As a final example for working with lists, let's implement a `hasSubsequence` function for checking whether a `List`
   * contains another `List` as a subsequence. For instance, `List(1, 2, 3, 4)` would have `List(1, 2)`, `List(2, 3)`
   * and `List(4)` as subsequences, among others:
   *
   * {{{
   *   @annotation.tailrec
   *     def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
   *       case (_,Nil) => true
   *       case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
   *       case _ => false
   *     }
   *
   *   @annotation.tailrec
   *   def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
   *     case Nil => sub == Nil
   *     case _ if startsWith(sup, sub) => true
   *     case Cons(h,t) => hasSubsequence(t, sub)
   *   }
   * }}}
   *
   * Take a thorough look at the implementation of this function, and then try it out in the next exercise:
   */
  def listHasSubsequenceAssert(res0: Boolean, res1: Boolean, res2: Boolean): Unit = {
    def l = List(1, 2, 3, 4, 5)

    hasSubsequence(l, List(2, 3)) shouldBe res0
    hasSubsequence(l, List(0, 1)) shouldBe res1
    hasSubsequence(l, Nil) shouldBe res2
  }

  /**
   * = Trees =
   *
   * <b>Exercise 3.25:</b>
   *
   * Let's try to implement a function `size` to count the number of nodes (leaves and branches) in a tree:
   */
  def treeSizeAssert(res0: Int, res1: Int): Unit = {
    def size[A](t: Tree[A]): Int =
      t match {
        case Leaf(_)      => res0
        case Branch(l, r) => res1 + size(l) + size(r)
      }

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    size(t) shouldBe 5
  }

  /**
   * <b>Exercise 3.26:</b>
   *
   * Following a similar implementation, we can write a function `maximum` that returns the maximum element in a
   * Tree[Int]:
   *
   * {{{
   *   def maximum(t: Tree[Int]): Int = t match {
   *     case Leaf(n) => n
   *     case Branch(l,r) => maximum(l) max maximum(r)
   *   }
   * }}}
   *
   * <b>Exercise 3.27:</b>
   *
   * In the same fashion, let's implement a function `depth` that returns the maximum path length from the root of a
   * tree to any leaf.
   */
  def treeDepthAssert(res0: Int, res1: Int): Unit = {
    def depth[A](t: Tree[A]): Int =
      t match {
        case Leaf(_)      => res0
        case Branch(l, r) => res1 + (depth(l) max depth(r))
      }
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    depth(t) shouldBe 2
  }

  /**
   * <b>Exercise 3.28:</b>
   *
   * We can also write a function `map`, analogous to the method of the same name on `List`, that modifies each element
   * in a tree with a given function. Let's try it out in the following exercise:
   */
  def treeMapAssert(res0: Branch[Int]): Unit = {
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      t match {
        case Leaf(a)      => Leaf(f(a))
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
      }

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.map(t)(_ * 2) shouldBe res0
  }

  /**
   * <b>Exercise 3.29:</b>
   *
   * To wrap this section up, let's generalize `size`, `maximum`, `depth` and `map`, writing a new function `fold` that
   * abstracts over their similarities:
   *
   * {{{
   *   def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
   *     case Leaf(a) => f(a)
   *     case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
   *   }
   * }}}
   *
   * Let's try to reimplement `size`, `maximum`, `depth`, and `map` in terms of this more general function:
   */
  def treeFoldAssert(res0: Int, res1: Int, res2: Int, res3: Int, res4: Branch[Boolean]): Unit = {
    def sizeViaFold[A](t: Tree[A]): Int =
      fold(t)(a => res0)(res1 + _ + _)

    def maximumViaFold(t: Tree[Int]): Int =
      fold(t)(a => a)(_ max _)

    def depthViaFold[A](t: Tree[A]): Int =
      fold(t)(a => res2)((d1, d2) => res3 + (d1 max d2))

    def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    sizeViaFold(t) shouldBe 5
    maximumViaFold(t) shouldBe 3
    depthViaFold(t) shouldBe 2
    mapViaFold(t)(_ % 2 == 0) shouldBe res4
  }
}
