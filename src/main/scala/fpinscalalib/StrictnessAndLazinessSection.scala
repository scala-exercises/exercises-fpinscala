package fpinscalalib

import fpinscalalib.customlib.laziness._
import fpinscalalib.customlib.laziness.Stream
import fpinscalalib.customlib.laziness.Stream._
import org.scalatest.{FlatSpec, Matchers}
import fpinscalalib.customlib.laziness.ExampleHelper._

/** @param name sctriness_and_laziness
  */
object StrictnessAndLazinessSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {

  /**
    * = Strict and non-strict functions =
    *
    * Non-strictness is a property of a function. To say a function is `non-strict` just means that the function may
    * choose not to evaluate one or more of its arguments. In contrast, a `strict` function always evaluates its
    * arguments.  Unless we tell it otherwise, any function definition in Scala will be strict. As an example, consider
    * the following function:
    *
    * {{{
    *   def square(x: Double): Double = x * x
    * }}}
    *
    * When you invoke `square(41.0 + 1.0)`, the function `square` will receive the evaluated value of `42.0` because
    * it's strict. If you invoke `square(sys.error("failure"))`, you’ll get an exception before square has a chance to do
    * anything, since the `sys.error("failure")` expression will be evaluated before entering the body of square.
    *
    * For example, the short-circuiting Boolean functions `&&` and `||`, found in many programming languages including
    * Scala, are non-strict. You can think of them as functions that may choose not to evaluate their arguments. The
    * function `&&` takes two `Boolean` arguments, but only evaluates the second argument if the first is `true`:
    *
    * {{{
    *   false && { println("!!"); true } // does not print anything
    * }}}
    *
    * And `||` only evaluates its second argument if the first is `false`:
    *
    * {{{
    *   true || { println("!!"); false } // doesn't print anything either
    * }}}
    *
    * Another example of non-strictness is the `if` control construct in Scala. It can be thought of as a function
    * accepting three parameters: a condition of type `Boolean`, an expression of some type `A` to return in the case that
    * the condition is `true`, and another expression of the same type `A` to return if the condition is `false`. We'd
    * say that the if function is strict in its condition parameter, since it’ll always evaluate the condition to
    * determine which branch to take, and non-strict in the two branches for the `true` and `false` cases,
    * since it’ll only evaluate one or the other based on the condition. We can re-implement it the following way:
    *
    * {{{
    *   def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A =
    *     if (cond) onTrue() else onFalse()
    * }}}
    *
    *
    *
    * Play a little bit with it, note what happens when you force a `true` or a `false` condition on the `if2` call:
    */

  def if2Assert(res0: Boolean): Unit = {
    if2(res0, () => true, () => { sys.error("Exception occurred: if2 call went through the false branch") })
  }

  /**
    * The arguments we’d like to pass unevaluated have a `() =>` immediately before their type. A value of type
    * `() => A` is a function that accepts zero arguments and returns an `A`. In general, the unevaluated form of an
    * expression is called a `thunk`, and we can force the thunk to evaluate the expression and get a result. We do
    * so by invoking the function, passing an empty argument list, as in `onTrue()` or `onFalse()`. Likewise, callers
    * of `if2` have to explicitly create thunks, and the syntax follows the same conventions as the function literal
    * syntax we’ve already seen. But this is such a common case that Scala provides some nicer syntax:
    *
    * {{{
    *   def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    *     if (cond) onTrue else onFalse
    *
    *   if2(false, sys.error("fail"), 3)
    * }}}
    *
    * With either syntax, an argument that’s passed unevaluated to a function will be evaluated once for each place
    * it’s referenced in the body of the function. That is, Scala won’t (by default) cache the result of evaluating an
    * argument:
    *
    * {{{
    *   def maybeTwice(b: Boolean, i: => Int) = if (b) i+i else 0
    *
    *   scala> val x = maybeTwice(true, { println("hi"); 1 + 41 })
    *   hi
    *   hi
    *   x: Int = 84
    * }}}
    *
    * Here, i is referenced twice in the body of maybeTwice, and we’ve made it particularly obvious that it’s evaluated
    * each time by passing the block `{ println("hi"); 1+41 }`, which prints hi as a side effect before returning a
    * result of `42`. The expression `1 + 41` will be computed twice as well. We can cache the value explicitly if we
    * wish to only evaluate the result once, by using the `lazy` keyword:
    *
    * {{{
    *   def maybeTwice2(b: Boolean, i: => Int) = {
    *     lazy val j = i
    *     if (b) j + j else 0
    *   }
    *
    *   scala> val x = maybeTwice2(true, { println("hi"); 1 + 41 })
    *   hi
    *   x: Int = 84
    * }}}
    *
    * Adding the `lazy` keyword to a `val` declaration will cause Scala to delay evaluation of the right-hand side of that
    * `lazy val` declaration until it’s first referenced. It will also cache the result so that subsequent references to
    * it don’t trigger repeated evaluation.
    *
    * = Lazy lists =
    *
    * We’ll explore how laziness can be used to improve the efficiency and modularity of functional programs using lazy
    * lists, or streams, as an example.
    *
    * {{{
    *   sealed trait Stream[+A]
    *   case object Empty extends Stream[Nothing]
    *   case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
    *
    *   object Stream {
    *     def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    *       lazy val head = hd
    *       lazy val tail = tl
    *       Cons(() => head, () => tail)
    *     }
    *     def empty[A]: Stream[A] = Empty
    *
    *     def apply[A](as: A*): Stream[A] =
    *       if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
    *   }
    * }}}
    *
    * This type looks identical to our List type, except that the Cons data constructor takes `explicit` thunks
    * `(() => A and () => Stream[A])` instead of regular strict values. If we wish to examine or traverse the Stream,
    * we need to force these thunks as we did earlier in our definition of `if2`. For example:
    *
    * {{{
    *   def headOption: Option[A] = this match {
    *     case Empty => None
    *     case Cons(h, t) => Some(h())  // explicit forcing of h using h()
    *   }
    * }}}
    *
    *
    * Now let's write a few helper functions to make inspecting streams easier, starting with a function to convert a
    * `Stream` to a `List` (which will force its evaluation):
    */

  def streamToListAssert(res0: List[Int]): Unit = {
    def toList[A](s: Stream[A]): List[A] = s match {
      case Cons(h,t) => h() :: t().toListRecursive
      case _ => List()
    }

    val s = Stream(1, 2, 3)
    toList(s) shouldBe res0
  }

  /**
    * Let's continue by writing the function `take` for returning the fist `n` elements of a `Stream`. Note that in the
    * following implementation we're using `Stream`'s smart constructors `cons` and `empty`, defined as follows:
    *
    * {{{
    *   def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    *     lazy val head = hd
    *     lazy val tail = tl
    *     Cons(() => head, () => tail)
    *   }
    *
    *   def empty[A]: Stream[A] = Empty
    * }}}
    */

  def streamTakeAssert(res0: Int): Unit = {
    def take[A](s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(h, t) if n > 0 => cons[A](h(), t().take(n - res0))
      case Cons(h, _) if n == 0 => cons[A](h(), Stream.empty)
      case _ => Stream.empty
    }

    take(Stream(1, 2, 3), 2).toList shouldBe List(1, 2)
  }

  /**
    * `drop` is similar to `take`, but instead skips the first `n` elements of a `Stream`:
    */

  def streamDropAssert(res0: Int): Unit = {
    def drop[A](s: Stream[A], n: Int): Stream[A] = s match {
      case Cons(_, t) if n > 0 => t().drop(n - res0)
      case _ => s
    }

    drop(Stream(1, 2, 3, 4, 5), 2).toList shouldBe List(3, 4, 5)
  }

  /**
    * We can also implement `takeWhile` for returning all starting elements of a `Stream` that match the given predicate:
    */

  def streamTakeWhileAssert(res0: List[Int], res1: List[Int]): Unit = {
    def takeWhile[A](s: Stream[A], f: A => Boolean): Stream[A] = s match {
      case Cons(h,t) if f(h()) => cons(h(), t() takeWhile f)
      case _ => Stream.empty
    }

    takeWhile(Stream(1, 2, 3, 4, 5), (x: Int) => x < 3).toList shouldBe res0
    takeWhile(Stream(1, 2, 3, 4, 5), (x: Int) => x < 0).toList shouldBe res1
  }

  /**
    * = Separating program description from evaluation =
    *
    * Laziness lets us separate the description of an expression from the evaluation of that expression. This gives us
    * a powerful ability — we may choose to describe a “larger” expression than we need, and then evaluate only a portion
    * of it. As an example, let’s look at the function exists that checks whether an element matching a `Boolean`
    * function exists in this Stream:
    *
    * {{{
    *   def exists(p: A => Boolean): Boolean = this match {
    *     case Cons(h, t) => p(h()) || t().exists(p)
    *     case _ => false
    *   }
    * }}}
    *
    * Note that `||` is non-strict in its second argument. If `p(h())` returns `true`, then exists terminates the
    * traversal early and returns true as well. Remember also that the tail of the stream is a `lazy val`. So not only
    * does the traversal terminate early, the tail of the stream is never evaluated at all!
    *
    * We can implement a `foldRight` similar to that of `List`, but expressed in a lazy way:
    *
    * {{{
    *   def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    *     case Cons(h,t) => f(h(), t().foldRight(z)(f))
    *     case _ => z
    *   }
    * }}}
    *
    * This looks very similar to the `foldRight` we wrote for `List`, but note how our combining function `f` is
    * non-strict in its second parameter. If `f` chooses not to evaluate its second parameter, this terminates the
    * traversal early. We can see this by using `foldRight` to implement `exists`:
    *
    * {{{
    *   def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)
    * }}}
    *
    * Let's use this to implement `forAll`, which checks that all elements in the `Stream` match a given predicate. Note
    * that the implementation will terminate the traversal as soon as it encounters a nonmatching value.
    */

  def streamForAllAssert(res0: Boolean): Unit = {
    def forAll[A](s: Stream[A], f: A => Boolean): Boolean =
      s.foldRight(res0)((a, b) => f(a) && b)

    forAll(Stream(1, 2, 3), (x: Int) => x % 2 == 0) shouldBe false
    forAll(Stream("a", "b", "c"), (x: String) => x.size > 0) shouldBe true
  }

  /**
    * Let's put `foldRight` to good use, by implementing `takeWhile` based on it:
    *
    * {{{
    *   def takeWhile_1(f: A => Boolean): Stream[A] =
    *     foldRight(empty[A])((h,t) =>
    *       if (f(h)) cons(h,t)
    *       else      empty)
    * }}}
    *
    * We can also do the same with `headOption`:
    *
    * {{{
    *   def headOption: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))
    * }}}
    *
    * Implementations for `map`, `filter`, `append` and `flatMap` using `foldRight` should sound familiar already:
    *
    * {{{
    *   def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h), t))
    *
    *   def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((h,t) =>
    *     if (f(h)) cons(h, t)
    *     else t)
    *
    *   def append[B>:A](s: => Stream[B]): Stream[B] = foldRight(s)((h,t) => cons(h,t))
    *
    *   def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h,t) => f(h) append t)
    * }}}
    *
    * Note that these implementations are incremental — they don’t fully generate their answers. It’s not until some
    * other computation looks at the elements of the resulting `Stream` that the computation to generate that `Stream`
    * actually takes place. Let's look at a simplified program trace for the next piece of code.
    *
    * {{{
    *   Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0)
    * }}}
    *
    * We'll convert that expression to a `List` to force evaluation. Try to follow what's happening in each step:
    */

  def streamTraceAssert(res0: Int, res1: Stream[Int], res2: Stream[Int], res3: Int, res4: Stream[Int], res5: Int): Unit = {
    val startingPoint = Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList

    // Apply map to the first element:
    val step1 = cons(res0, Stream(2,3,4).map(_ + 10)).filter(_ % 2 == 0).toList
    // Apply filter to the first element:
    val step2 = res1.map(_ + 10).filter(_ % 2 == 0).toList
    // Apply map to the second element:
    val step3 = cons(12, res2.map(_ + 10)).filter(_ % 2 == 0).toList
    // Apply filter to the second element. Produce the first element of the result:
    val step4 = 12 :: Stream(3,4).map(_ + 10).filter(_ % 2 == 0).toList
    val step5 = 12 :: cons(res3, res4.map(_ + 10)).filter(_ % 2 == 0).toList
    val step6 = 12 :: Stream(4).map(_ + 10).filter(_ % 2 == 0).toList
    val step7 = 12 :: cons(res5, Stream[Int]().map(_ + 10)).filter(_ % 2 == 0).toList
    // Apply filter to the fourth element and produce the final element of the result.
    val step8 = 12 :: 14 :: Stream[Int]().map(_ + 10).filter(_ % 2 == 0).toList
    // map and filter have no more work to do, and the empty stream becomes the empty list.
    val finalStep = 12 :: 14 :: List()

    startingPoint shouldBe step1
    step1 shouldBe step2
    step2 shouldBe step3
    step3 shouldBe step4
    step4 shouldBe step5
    step5 shouldBe step6
    step6 shouldBe step7
    step7 shouldBe step8
    step8 shouldBe finalStep
  }

  /**
    * = Infinite streams and corecursion =
    *
    * Because they’re incremental, the functions we’ve written also work for `infinite streams`. Here’s an example of
    * an infinite `Stream` of `1`s:
    *
    * {{{
    *   val ones: Stream[Int] = Stream.cons(1, ones)
    * }}}
    *
    * Although ones is infinite, the functions we’ve written so far only inspect the portion of the stream needed to
    * generate the demanded output. For example:
    */

  def streamOnesAssert(res0: List[Int], res1: Boolean, res2: Boolean, res3: Boolean): Unit = {
    ones.take(5).toList shouldBe res0
    ones.exists(_ % 2 != 0) shouldBe res1
    ones.map(_ + 1).exists(_ % 2 == 0) shouldBe res2
    ones.forAll(_ != 1) shouldBe res3
  }

  /**
    * Let's generalize `ones` slightly to the function `constant`, which returns an infinite `Stream` of a given value:
    *
    * {{{
    *   def constant[A](a: A): Stream[A] = {
    *     lazy val tail: Stream[A] = Cons(() => a, () => tail)
    *     tail
    *   }
    * }}}
    *
    * Of course, we can generate `number series` with `Stream`s. For example, let's write a function that generates an
    * infinite stream of integers (n, n + 1, n + 2...):
    */

  def streamIntegersAssert(res0: Int): Unit = {
    def from(n: Int): Stream[Int] =
      cons(n, from(n + res0))

    from(100).take(5).toList shouldBe List(100, 101, 102, 103, 104)
  }

  /**
    * We can also create a function `fibs` that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8...
    */

  def streamFibsAssert(res0: Int, res1: Int): Unit = {
    val fibs = {
      def go(f0: Int, f1: Int): Stream[Int] =
        cons(f0, go(f1, f0+f1))
      go(res0, res1)
    }

    fibs.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  /**
    * Now we're going to write a more general stream-building function called `unfold`. It takes an initial state, and
    * a function for producing both the next state and the next value in the generated stream:
    *
    * {{{
    *   def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    *     case Some((h,s)) => cons(h, unfold(s)(f))
    *     case None => empty
    *   }
    * }}}
    *
    * `Option` is used to indicate when the `Stream` should be terminated, if at all. Now that we have `unfold`, let's
    * re-write our previous generator functions based on it, starting from `fibs`:
    */

  def streamFibsViaUnfoldAssert(res0: Int, res1: Int): Unit = {
    val fibsViaUnfold =
      unfold((res0, res1)) { case (f0,f1) => Some((f0, (f1, f0+f1))) }

    fibsViaUnfold.take(7).toList shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  /**
    * `from` follows a similar principle, albeit a little bit more simple:
    */

  def streamFromViaUnfoldAssert(res0: Int): Unit = {
    def fromViaUnfold(n: Int) = unfold(n)(n => Some((n, n + res0)))

    fromViaUnfold(100).take(5).toList shouldBe List(100, 101, 102, 103, 104)
  }

  /**
    * Again, `constant` can be implemented in terms of `unfold` in a very similar way:
    *
    * {{{
    *   def constantViaUnfold[A](a: A) = unfold(a)(_ => Some((a,a)))
    * }}}
    *
    * Follow the same pattern to implement `ones` using `unfold`:
    */

  def streamOnesViaUnfoldAssert(res0: Some[(Int, Int)]): Unit = {
    val onesViaUnfold = unfold(1)(_ => res0)

    onesViaUnfold.take(10).toList shouldBe List(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
  }

  /**
    * Now we're going to re-implement some of the higher-order functions for `Stream`s, starting with map:
    *
    * {{{
    *   def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    *     case Cons(h,t) => Some((f(h()), t()))
    *     case _ => None
    *     }
    * }}}
    *
    * `take` can also be re-written via `unfold`, let's try it:
    */

  def streamTakeViaUnfold(res0: Int, res1: Int): Unit = {
    def takeViaUnfold[A](s: Stream[A], n: Int): Stream[A] =
      unfold((s, n)) {
        case (Cons(h,t), 1) => Some((h(), (Stream.empty, res0)))
        case (Cons(h,t), n) if n > 1 => Some((h(), (t(), n - res1)))
        case _ => None
      }

    takeViaUnfold(Stream(1, 2, 3, 4, 5), 5).toList shouldBe List(1, 2, 3, 4, 5)
  }

  /**
    * Let's continue by re-writting `takeWhile` in terms of `unfold`:
    *
    * {{{
    *   def takeWhileViaUnfold(f: A => Boolean): Stream[A] = unfold(this) {
    *     case Cons(h,t) if f(h()) => Some((h(), t()))
    *     case _ => None
    *   }
    * }}}
    *
    * We can also bring back functions we saw with `List`s, as `zipWith`:
    *
    * {{{
    *   def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)) {
    *     case (Cons(h1,t1), Cons(h2,t2)) =>
    *       Some((f(h1(), h2()), (t1(), t2())))
    *     case _ => None
    *   }
    * }}}
    *
    * `zipAll` can also be implemented using `unfold`. Note that it should continue the traversal as long as either
    * stream has more elements - it uses `Option` to indicate whether each stream has been exhausted:
    *
    * {{{
    *   def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = zipWithAll(s2)((_,_))
    *
    *   def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    *     Stream.unfold((this, s2)) {
    *       case (Empty, Empty) => None
    *       case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
    *       case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
    *       case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
    *   }
    * }}}
    *
    * Now we're going to try to implement `startsWith` using the functions we've seen so far:
    *
    * {{{
    *   def startsWith[A](s: Stream[A]): Boolean =
    *     zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    *       case (h,h2) => h == h2
    *     }
    * }}}
    *
    * We can also write `tails` using `unfold`. For a given Stream, `tails` returns the `Stream` of suffixes of the input
    * sequence, starting with the original `Stream`. For example, given `Stream(1,2,3)`, it would return
    * `Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())`.
    */

  def streamTailsAssert(res0: Int): Unit = {
    def tails[A](s: Stream[A]): Stream[Stream[A]] =
      unfold(s) {
        case Empty => None
        case s1 => Some((s1, s1 drop res0))
      } append Stream(Stream.empty)

    tails(Stream(1, 2, 3)).toList.map(_.toList) shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }

  /**
    * We can generalize tails to the function scanRight, which is like a `foldRight` that returns a stream of the
    * intermediate results:
    *
    * {{{
    *   def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    *     foldRight((z, Stream(z)))((a, p0) => {
    *       lazy val p1 = p0
    *       val b2 = f(a, p1._1)
    *       (b2, cons(b2, p1._2))
    *    })._2
    * }}}
    *
    * For example:
    */

  def streamScanRightAssert(res0: List[Int]): Unit = {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList shouldBe res0
  }
}
