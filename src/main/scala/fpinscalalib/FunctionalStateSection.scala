/*
 *  scala-exercises - exercises-fpinscala
 *  Copyright (C) 2015-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
 */

package fpinscalalib

import fpinscalalib.customlib.state.RNG.Simple
import fpinscalalib.customlib.state.RNG._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import fpinscalalib.customlib.state._

/** @param name pure_functional_state
 */
object FunctionalStateSection
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
   * = Purely functional random number generation =
   *
   * <b>Exercise 6.1:</b>
   *
   * Let's write a function that uses `RNG.nextInt` to generate a random integer between `0` and `Int.maxValue`, making
   * sure to handle the corner case when `nextInt` returns `Int.MinValue`, which doesn't have a non-negative
   * counterpart:
   */
  def randomNonNegativeIntAssert(res0: Int, res1: Int): Unit = {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < res0) -(i + res1) else i, r)
    }

    val rng             = Simple(47)
    val (result1, rng1) = nonNegativeInt(rng)
    val result2         = nonNegativeInt(rng1)._1

    result1 should be >= 0
    result2 should be >= 0
    result1 should not be result2
  }

  /**
   * <b>Exercise 6.2:</b>
   *
   * Now let's write a function to generate a `Double` between `0` and `1`, excluding `1`. Note that we use
   * `Int.MaxValue` to divide a random positive integer:
   */
  def randomDoubleAssert(res0: Int): Unit = {
    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + res0), r)
    }

    val rng             = Simple(47)
    val (double1, rng1) = double(rng)
    val double2         = double(rng1)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  /**
   * <b>Exercise 6.3:</b>
   *
   * Following the same principle, we're going to write functions that generate tuples of random values, i.e.: an
   * `(Int, Double)` pair, a `(Double, Int)` pair, and a `(Double, Double, Double)` 3-tuple, by reusing the functions
   * we've already written:
   *
   * {{{
   *   def intDouble(rng: RNG): ((Int, Double), RNG) = {
   *     val (i, r1) = rng.nextInt
   *     val (d, r2) = double(r1)
   *     ((i, d), r2)
   *   }
   *
   *   def doubleInt(rng: RNG): ((Double, Int), RNG) = {
   *     val ((i, d), r) = intDouble(rng)
   *     ((d, i), r)
   *   }
   *
   *   def double3(rng: RNG): ((Double, Double, Double), RNG) = {
   *     val (d1, r1) = double(rng)
   *     val (d2, r2) = double(r1)
   *     val (d3, r3) = double(r2)
   *     ((d1, d2, d3), r3)
   *   }
   * }}}
   *
   * <b>Exercise 6.4:</b>
   *
   * We can even go a bit beyond and write a function to generate a list of random integers:
   */
  def randomIntListAssert(res0: Int, res1: Int): Unit = {
    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
      if (count == res0)
        (List(), rng)
      else {
        val (x, r1)  = rng.nextInt
        val (xs, r2) = ints(count - res1)(r1)
        (x :: xs, r2)
      }

    val (list1, rng1) = ints(5)(Simple(47))
    val list2         = ints(5)(rng1)._1
    list1.size shouldBe 5
    list1.headOption should not be list2
  }

  /**
   * = A better API for state actions =
   *
   * <b>Exercise 6.5:</b>
   *
   * Let's use `map` to reimplement `double` in a more elegant way:
   */
  def randomDoubleViaMap(res0: Int): Unit = {
    val double: Rand[Double] =
      map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + res0))

    val rng             = Simple(47)
    val (double1, rng2) = double(rng)
    val double2         = double(rng2)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  /**
   * <b>Exercise 6.6:</b>
   *
   * Now we're going to implement `map2` which takes two actions, `ra` and `rb`, and a binary function `f` for combining
   * their results, and returns a new action that combines them:
   *
   * {{{
   *   def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
   *     rng => {
   *       val (a, r1) = ra(rng)
   *       val (b, r2) = rb(r1)
   *       (f(a, b), r2)
   *     }
   * }}}
   *
   * <b>Exercise 6.7:</b>
   *
   * If we can combine two `RNG` transitions, we should be able to combine a whole list of them. Let's implement
   * `sequence` for combining a `List` of transitions into a single transition:
   *
   * {{{
   *   def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
   *     fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
   * }}}
   *
   * <b>Exercise 6.8:</b>
   *
   * Our next step is to implement `flatMap`, as it will allow us to change state operations:
   *
   * {{{
   *   def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
   *     rng => {
   *       val (a, r1) = f(rng)
   *       g(a)(r1)
   *     }
   * }}}
   *
   * Let's use it to write `nonNegativeLessThan`, which generates an integer between `0` (inclusive) and `n`
   * (exclusive):
   */
  def randomNonNegativeLessThan(res0: Int, res1: Int): Unit = {
    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n - res0) - mod >= res1) unit(mod) else nonNegativeLessThan(n)
      }
    }

    val (result1, rng1) = nonNegativeLessThan(10)(Simple(47))
    val result2         = nonNegativeLessThan(10)(rng1)._1

    result1 should be >= 0
    result1 should be < 10
    result2 should be >= 0
    result2 should be < 10
    result1 should not be result2
  }

  /**
   * <b>Exercise 6.10:</b>
   *
   * We can also rewrite `map` and `map2` in terms of `flatMap`:
   *
   * {{{
   *   def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
   *     flatMap(s)(a => unit(f(a)))
   *    def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
   *     flatMap(ra)(a => map(rb)(b => f(a, b)))
   * }}}
   *
   * <b>Exercise 6.x:</b>
   *
   * As a final example, let’s revisit the functions we previously wrote and implement a function that will roll a
   * six-sided die:
   */
  def randomRollDie(res0: Int): Unit = {
    def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + res0)

    val (dice1, rng1) = rollDie(Simple(47))
    val dice2         = rollDie(rng1)._1
    dice1 should be > 0
    dice1 should be < 6
    dice2 should be > 0
    dice2 should be < 6
    dice1 should not be dice2
  }

  import fpinscalalib.customlib.state.Machine
  import fpinscalalib.customlib.state.State
  import fpinscalalib.customlib.state.State._

  /**
   * = A general state action data type =
   *
   * <b>Exercise 6.10:</b>
   *
   * Let's generalize the functions `unit`, `map`, `map2`, `flatMap` and `sequence`:
   *
   * {{{
   *   def unit[S, A](a: A): State[S, A] = State(s => (a, s))
   *
   *   def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))
   *
   *   def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
   *     val (a, s1) = run(s)
   *     f(a).run(s1)
   *   })
   *
   *   def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
   *     flatMap(a => sb.map(b => f(a, b)))
   *
   *   def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] =
   *     sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))
   * }}}
   *
   * <b>Exercise 6.11:</b>
   *
   * As a final showcase of the uses of `State`, let's implement a finite state automaton that models a simple candy
   * dispenser. The machine has two inputs: you can insert a coin, or you can dispense candy by turning the knob.
   * It can be in one of two states: locked or unlocked. It also tracks how many candies are left and how many coins
   * it contains.
   *
   * {{{
   *   sealed trait Input
   *   case object Coin extends Input
   *   case object Turn extends Input
   *   case class Machine(locked: Boolean, candies: Int, coins: Int)
   * }}}
   *
   * The rules of the machine are:
   *
   * - Inserting a coin into a locked machine will unlock it if there’s still any candy left.
   * - Turning the knob on an unlocked machine will cause it to dispense one candy and return to a locked state.
   * - Turning the knob on a locked machine or inserting a coin into an unlocked machine has no effect.
   * - A machine that’s out of candy ignores any kind of input.
   *
   * The method `simulateMachine` should operate the machine based on a list of inputs and return the number of coins
   * and candies left in the machine. For instance, if the input `Machine` has 10 coins and 5 candies, and a
   * total of 4 candies are bought successfully, the output should be `(14, 1)`.
   */
  def candyMachineAssert(res0: Int, res1: Int): Unit = {
    object Candy {
      def update =
        (i: Input) =>
          (s: Machine) =>
            (i, s) match {
              case (_, Machine(_, 0, _))        => s
              case (Coin, Machine(false, _, _)) => s
              case (Turn, Machine(true, _, _))  => s
              case (Coin, Machine(true, candy, coin)) =>
                Machine(false, candy, coin + res0)
              case (Turn, Machine(false, candy, coin)) =>
                Machine(true, candy - res1, coin)
        }

      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
        for {
          _ <- sequence(inputs map (modify[Machine] _ compose update))
          s <- get
        } yield (s.coins, s.candies)
    }

    import Candy._

    val inputCoin = List(Coin)
    val inputTurn = List(Turn)

    // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    val machine1 = Machine(true, 1, 0)
    simulateMachine(inputCoin).run(machine1)._2.locked shouldBe false

    // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    val machine2 = Machine(false, 1, 1)
    val m2Result = simulateMachine(inputTurn).run(machine2)
    m2Result._2.locked shouldBe true
    m2Result._2.candies shouldBe 0

    // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    simulateMachine(inputTurn).run(machine1)._2.locked shouldBe machine1.locked
    simulateMachine(inputCoin).run(machine2)._2.locked shouldBe machine2.locked

    // A machine that’s out of candy ignores all inputs.
    val machine3 = Machine(true, 0, 1)
    simulateMachine(inputTurn).run(machine3)._2.locked shouldBe machine3.locked
    simulateMachine(inputCoin).run(machine3)._2.locked shouldBe machine3.locked
  }
}
