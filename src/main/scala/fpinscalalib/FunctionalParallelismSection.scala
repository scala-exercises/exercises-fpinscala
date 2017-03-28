/*
 * scala-exercises - exercises-fpinscala
 * Copyright (C) 2015-2016 47 Degrees, LLC. <http://www.47deg.com>
 */

package fpinscalalib

import java.util.concurrent.ExecutorService

import fpinscalalib.customlib.functionalparallelism.Par
import fpinscalalib.customlib.functionalparallelism.Par._
import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent.Executors

/** @param name purely_functional_parallelism
 */
object FunctionalParallelismSection
    extends FlatSpec
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
   * = A data type for parallel computations =
   *
   * <b>Exercise 7.1:</b>
   *
   * `Par.map2` is a new higher-order function for combining the result of two parallel computations. Its signature is
   * as follows:
   *
   * {{{
   *    def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]
   * }}}
   *
   * = Refining the API =
   *
   * <b>Exercise 7.3:</b>
   *
   * Let's fix the implementation of `map2` so that it respects the contract of timeouts on `Future`:
   *
   * {{{
   *   def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
   *     es => {
   *       val (af, bf) = (a(es), b(es))
   *       Map2Future(af, bf, f)
   *     }
   * }}}
   *
   * <b>Exercise 7.4:</b>
   *
   * Let's create some operations! Using `lazyUnit`, let's write a function to convert any function `A => B` to one
   * that evaluates its result asynchronously:
   */
  def parAsyncFAssert(res0: String): Unit = {
    def asyncF[A, B](f: A => B): A => Par[B] =
      a => lazyUnit(f(a))

    def asyncIntToString = asyncF((x: Int) => x.toString())
    val executorService  = Executors.newFixedThreadPool(2)

    Par.run(executorService)(asyncIntToString(10)).get() shouldBe res0
  }

  /**
   * <b>Exercise 7.5:</b>
   *
   * Remember, `asyncF` converts an `A => B` to an `A => Par[B`] by forking a parallel computation to produce the
   * result. So we can fork off our N parallel computations pretty easily, but we need some way of collecting their
   * results. Are we stuck? Well, just from inspecting the types, we can see that we need some way of converting our
   * `List[Par[B]]` to the `Par[List[B]]` required by the return type of `parMap`.
   *
   * Let's try to write the function `sequence` that will allow us convert a `List[Par[B]]` into a `Par[List[B]]`:
   *
   * {{{
   *   def sequence_simple[A](l: List[Par[A]]): Par[List[A]] =
   *     l.foldRight[Par[List[A]]](unit(List()))((h,t) => map2(h,t)(_ :: _))
   * }}}
   *
   * <b>Exercise 7.6:</b>
   *
   * We can also implement `parFilter`, a function that filters elements of a list in parallel:
   *
   * {{{
   *   def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
   *     val pars: List[Par[List[A]]] =
   *     l map asyncF((a: A) => if (f(a)) List(a) else List())
   *     map(sequence(pars))(_.flatten)
   *   }
   * }}}
    **/
  def parFilterAssert(res0: List[Int]): Unit = {
    def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
      val pars: List[Par[List[A]]] =
        l map (asyncF((a: A) => if (f(a)) List(a) else List()))
      map(sequence(pars))(_.flatten)
    }

    val filterOp        = parFilter(List(1, 2, 3, 4, 5))(_ < 4)
    val executorService = Executors.newCachedThreadPool()
    val result          = Par.run(executorService)(filterOp).get()
    result shouldBe res0
  }

  /**
   * = The algebra of an API =
   *
   * <b>Exercise 7.9</b>
   *
   * For a thread pool of size 2, `fork(fork(fork(x)))` will deadlock, and so on. Another, perhaps more interesting
   * example is `fork(map2(fork(x), fork(y)))`. In this case, the outer task is submitted first and occupies a thread
   * waiting for both `fork(x)` and `fork(y)`. The `fork(x)` and `fork(y)` tasks are submitted and run in parallel,
   * except that only one thread is available, resulting in deadlock.
   *
   * = Refining combinators to their most general form =
   *
   * <b>Exercise 7.11</b>
   *
   * Let’s implement `choiceN`, that will allow us to choose between an arbitrary list of parallel computations based
   * on the result of a given first:
   *
   * {{{
   *   def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
   *     es => {
   *       val ind = run(es)(n).get
   *       run(es)(choices(ind))
   *     }
   * }}}
   *
   * Let's try to implement `choice` via the more general `choiceN`:
   */
  def parChoiceNAssert(res0: Int): Unit = {
    def choiceViaChoiceN[A](a: Par[Boolean])(ifTrue: Par[A], ifFalse: Par[A]): Par[A] =
      choiceN(map(a)(b => if (b) 0 else res0))(List(ifTrue, ifFalse))

    val executorService = Executors.newFixedThreadPool(2)
    val choice          = choiceViaChoiceN(Par.unit(true))(Par.unit(1), Par.unit(2))
    choice.apply(executorService).get() shouldBe 1
  }

  /**
   * <b>Exercise 7.12:</b>
   *
   * Instead of a list of computations, let's use a `Map` of them:
   */
  def parChoiceMapAssert(res0: Int): Unit = {
    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      es => {
        val k = Par.run(es)(key).get
        Par.run(es)(choices(k))
      }

    val executorService = Executors.newFixedThreadPool(2)
    val choicesMap      = Map("a" -> Par.unit(1), "b" -> Par.unit(2))

    choiceMap(Par.unit("b"))(choicesMap).apply(executorService).get() shouldBe res0
  }

  /**
   * <b>Exercise 7.13:</b>
   *
   * Let’s make a more general function that unifies them all:
   */
  def parChooserAssert(res0: String): Unit = {
    def chooser[A, B](p: Par[A])(choices: A => Par[B]): Par[B] =
      es => {
        val k = Par.run(es)(p).get
        Par.run(es)(choices(k))
      }

    val choices = (a: Int) => {
      if (a % 2 == 0) Par.unit("even")
      else Par.unit("odd")
    }

    val executorService = Executors.newFixedThreadPool(2)
    chooser(Par.unit(1))(choices).apply(executorService).get() shouldBe res0
  }

  /**
   * This new primitive `chooser` is usually called `bind` or `flatMap`. Let's use it to re-implement `choice`:
   */
  def parChoiceViaFlatMapAssert(res0: String): Unit = {
    def choiceViaFlatMap[A](p: Par[Boolean])(f: Par[A], t: Par[A]): Par[A] =
      flatMap(p)(b => if (b) t else f)

    val executorService = Executors.newFixedThreadPool(2)
    val choice          = choiceViaFlatMap(Par.unit(false))(Par.unit("a"), Par.unit("b"))
    choice.apply(executorService).get() shouldBe res0
  }

  /**
   * We can also re-implement `choiceN` in terms of `flatMap`:
   */
  def parChoiceNViaFlatMapAssert(res0: String): Unit = {
    def choiceNViaFlatMap[A](p: Par[Int])(choices: List[Par[A]]): Par[A] =
      flatMap(p)(i => choices(i))

    val executorService = Executors.newFixedThreadPool(2)
    val choices         = List(Par.unit("a"), Par.unit("b"), Par.unit("c"))
    choiceNViaFlatMap(Par.unit(2))(choices).apply(executorService).get() shouldBe res0
  }

  /**
   * <b>Exercise 7.14:</b>
   *
   * `join` is a simpler combinator, for converting a Par[Par[X]] to Par[X] for any choice of X:
   *
   * {{{
   *   def join[A](a: Par[Par[A]]): Par[A] =
   *     es => run(es)(run(es)(a).get())
   * }}}
   *
   * We can implement `flatMap` using `join`, and vice-versa:
   *
   * {{{
   *   def flatMapViaJoin[A,B](p: Par[A])(f: A => Par[B]): Par[B] =
   *     join(map(p)(f))
   *
   *   def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
   *     flatMap(a)(x => x)
   * }}}
   *
   * Let's try this last combinator:
   */
  def parFlatMapJoinAssert(res0: String): Unit = {
    val nestedPar       = Par.unit(Par.unit("foo"))
    val executorService = Executors.newFixedThreadPool(2)

    joinViaFlatMap(nestedPar)(executorService).get() shouldBe res0
  }
}
