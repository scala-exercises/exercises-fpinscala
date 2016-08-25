package fpinscalalib

import fpinscalalib.customlib.state.RNG.Simple
import fpinscalalib.customlib.state.RNG._
import org.scalatest.{FlatSpec, Matchers}
import org.scalacheck.Shapeless._
import fpinscalalib.customlib.state._

/** @param name pure_functional_state
  */
object FunctionalStateSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {

  /**
    * == Purely functional random number generation =
    *
    * If you need to generate random numbers in Scala, there’s a class in the standard library, `scala.util.Random`,
    * with a pretty typical imperative API that relies on side effects. Here’s an example of its use:
    *
    * {{{
    *   val rng = new scala.util.Random
    *
    *   scala> rng.nextDouble
    *   res1: Double = 0.9867076608154569
    *   scala> rng.nextDouble
    *   res2: Double = 0.8455696498024141
    * }}}
    *
    * As this API doesn't guarantee referential transparency, we need to make the state updates `explicit`. Here's one
    * possible interface to a random number generator:
    *
    * {{{
    *   trait RNG {
    *     def nextInt: (Int, RNG)
    *   }
    * }}}
    *
    * The following is a simple random number generator that uses the same algorithm as `scala.util.Random`. The details
    * of this implementation aren’t really important, but notice that `nextInt` returns both the generated value and a
    * new `RNG` to use for generating the next value.
    *
    * {{{
    *   case class Simple(seed: Long) extends RNG {
    *     def nextInt: (Int, RNG) = {
    *       // `&` is bitwise AND. We use the current seed to generate a new seed.
    *       val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    *       // The next state, which is an `RNG` instance created from the new seed.
    *       val nextRNG = Simple(newSeed)
    *       // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    *       val n = (newSeed >>> 16).toInt
    *       // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    *       (n, nextRNG)
    *       }
    *    }
    * }}}
    *
    * Here's an example of using this API from the interpreter:
    *
    * {{{
    *   scala> val rng = SimpleRNG(42)
    *   rng: SimpleRNG = SimpleRNG(42)
    *
    *   scala> val (n1, rng2) = rng.nextInt
    *   n1: Int = 16159453
    *   rng2: RNG = SimpleRNG(1059025964525)
    *
    *   scala> val (n2, rng3) = rng2.nextInt
    *   n2: Int = -1281479697
    *   rng3: RNG = SimpleRNG(197491923327988)
    * }}}
    *
    * We can run this sequence of statements as many times as we want and we’ll always get the same values. When
    * we call `rng.nextInt`, it will always return `16159453` and a new `RNG`, whose `nextInt` will always return
    * `-1281479697`. In other words, this API is pure.
    *
    * = Making stateful APIs pure =
    *
    * Making seemingly stateful APIs pure is a problem that comes up frequently. We can always handle this in the same
    * way, for instance:
    *
    * {{{
    *   class Foo {
    *     private var s: FooState = ...
    *     def bar: Bar
    *     def baz: Int
    *   }
    * }}}
    *
    * Supposing `bar` and `baz` each mutate `s` in some way, we can translate this to the following alternative `trait`,
    * making the caller responsible for passing the computed next state through the rest of the program:
    *
    * {{{
    *   trait Foo {
    *     def bar: (Bar, Foo)
    *     def baz: (Int, Foo)
    *   }
    * }}}
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

    val rng = Simple(47)
    val (result1, rng1) = nonNegativeInt(rng)
    val result2 = nonNegativeInt(rng1)._1

    result1 should be >= 0
    result2 should be >= 0
    result1 should not be result2
  }

  /**
    * Now let's write a function to generate a `Double` between `0` and `1`, not including `1`. Note the use of
    * `Int.MaxValue` to divide a random positive integer:
    */

  def randomDoubleAssert(res0: Int): Unit = {
    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i / (Int.MaxValue.toDouble + res0), r)
    }

    val rng = Simple(47)
    val (double1, rng1) = double(rng)
    val double2 = double(rng1)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  /**
    * Following the same principle, we're going to write functions that generate tuples of random values, i.e.: an
    * `(Int, Double)` pair, a `(Double, Int)` pair, and a `(Double, Double, Double)` 3-tuple, by reusing the functions
    * we've already written:
    *
    * {{{
    *
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
    val list2 = ints(5)(rng1)._1
    list1.size shouldBe 5
    list1.headOption should not be list2
  }

  /**
    * To simplify the pattern we've been seeing so far, and to avoid passing the state along each time we execute these
    * state actions, let's make a type alias for the `RNG` state action data type:
    *
    * {{{
    *   type Rand[+A] = RNG => (A, RNG)
    * }}}
    *
    * We can now turn methods such as `RNG`'s `nextInt` into values of this new type:
    *
    * {{{
    *   val int: Rand[Int] = _.nextInt
    * }}}
    *
    * We want to write combinators that let us combine `Rand` actions while avoiding explicitly passing along the `RNG`
    * state. We’ll end up with a kind of domain-specific language that does all of the passing for us. For example, a
    * simple `RNG` state transition is the `unit` action, which passes the `RNG` state through without using it, always
    * returning a constant value rather than a random value:
    *
    * {{{
    *   def unit[A](a: A): Rand[A] = rng => (a, rng)
    * }}}
    *
    * There’s also `map` for transforming the output of a state action without modifying the state itself. Remember,
    * `Rand[A]` is just a type alias for a function type `RNG => (A, RNG)`, so this is just a kind of function
    * composition:
    *
    * {{{
    *   def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    *     val (a, rng2) = s(rng)
    *     (f(a), rng2)
    *   }
    * }}}
    *
    * As an example of how `map` is used, let’s write `nonNegativeEven`, which reuses `nonNegative Int` to generate an
    * `Int` that’s greater than or equal to zero and divisible by two:
    */

  def randomNonNegativeEvenAssert(res0: Int): Unit = {
    def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % res0)

    val (result1, rng1) = nonNegativeEven(Simple(47))
    val result2 = nonNegativeEven(rng1)._1

    result1 % 2 shouldBe 0
    result2 % 2 shouldBe 0
    result1 should not be result2
  }

  /**
    * We can also use `map` to reimplement `double` in a more elegant way:
    */

  def randomDoubleViaMap(res0: Int): Unit = {
    val double: Rand[Double] =
      map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + res0))

    val rng = Simple(47)
    val (double1, rng2) = double(rng)
    val double2 = double(rng2)._1

    double1.toInt should be >= 0
    double2.toInt should be >= 0
    double1 should not be double2
  }

  /**
    * Now we're going to implement `map2`, that will allow us to implement functions like `intDouble` and `doubleInt`.
    * This function takes two actions, `ra` and `rb`, and a binary function `f` for combining their results, and returns
    * a new action that combines them:
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
    * If we have an action that generates values of type `A` and an action to generate values of type `B`, then we can
    * combine them into one action that generates pairs of both `A` and `B`:
    *
    * {{{
    *   def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))
    * }}}
    *
    * We can use this to reimplement `intDouble` and `doubleInt` from exercise 6.3 more succinctly:
    *
    * {{{
    *   val randIntDouble: Rand[(Int, Double)] = both(int, double)
    *
    *   val randDoubleInt: Rand[(Double, Int)] = both(double, int)
    * }}}
    *
    * If we can combine two `RNG` transitions, we should be able to combine a whole list of them. Let's implement
    * `sequence` for combining a `List` of transitions into a single transition:
    *
    * {{{
    *   def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    *     fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))
    * }}}
    *
    * We can use `sequence` to reimplement the `ints` function we wrote before. To do so, let's use the standard library
    * function `List.fill(n)(x)` to make a list with `x` repeated `n` times:
    *
    * {{{
    *   def _ints(count: Int): Rand[List[Int]] =
    *     sequence(List.fill(count)(int))
    * }}}
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
    val result2 = nonNegativeLessThan(10)(rng1)._1

    result1 should be >= 0
    result1 should be < 10
    result2 should be >= 0
    result2 should be < 10
    result1 should not be result2
  }

  /**
    * We can also rewrite `map` and `map2` in terms of `flatMap`. The fact that this is possible is why we say that
    * `flatMap` is more powerful than `map` and `map2`.
    *
    * {{{
    *   def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    *     flatMap(s)(a => unit(f(a)))
    *    def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    *     flatMap(ra)(a => map(rb)(b => f(a, b)))
    * }}}
    *
    * As a final example, let’s revisit the functions we previously wrote and implement a function that will roll a
    * six-sided die:
    */

  def randomRollDie(res0: Int): Unit = {
    def rollDie: Rand[Int] = map(nonNegativeLessThan(6))(_ + res0)

    val (dice1, rng1) = rollDie(Simple(47))
    val dice2 = rollDie(rng1)._1
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
    * The functions we’ve just written — `unit`, `map`, `map2`, `flatMap`, and `sequence` — aren’t really specific to
    * random number generation at all. They’re general-purpose functions for working with state actions, and don’t care
    * about the type of the state. For instance, we can give `map` a more general signature:
    *
    * {{{
    *   def map[S,A,B](a: S => (A,S))(f: A => B): S => (B,S)
    * }}}
    *
    * This new version of `map` still retains the same implementation. We should then come up with a more general type
    * than `Rand`, for handling any type of state:
    *
    * {{{
    *   case class State[S,+A](run: S => (A,S))
    * }}}
    *
    * Now we have a single, general-purpose type and can use it to write general-purpose functions for capturing common
    * patterns of stateful programs. We can now  make `Rand` a type alias for `State`:
    *
    * {{{
    *   type Rand[A] = State[RNG, A]
    * }}}
    *
    * We can also generalize the functions `unit`, `map`, `map2`, `flatMap` and `sequence`:
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
    * As a final showcase of the uses of `State`, let's implement a finite state automaton that models a simple candy
    * dispenser. The machine has two types of input: you can insert a coin, or you can turn the knob to dispense candy.
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
    * The rules of the machine are as follows:
    *
    * - Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
    * - Turning the knob on an unlocked machine will cause it to dispense candy and then lock.
    * - Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    * - A machine that’s out of candy ignores all inputs.
    *
    * The method `simulateMachine` should operate the machine based on the list of inputs and return the number of coins
    * and candies left in the machine at the end. For example, if the input `Machine` has 10 coins and 5 candies, and a
    * total of 4 candies are successfully bought, the output should be `(14, 1)`.
    */

  def candyMachineAssert(res0: Int, res1: Int): Unit = {
    object Candy {
      def update = (i: Input) => (s: Machine) =>
        (i, s) match {
          case (_, Machine(_, 0, _)) => s
          case (Coin, Machine(false, _, _)) => s
          case (Turn, Machine(true, _, _)) => s
          case (Coin, Machine(true, candy, coin)) =>
            Machine(false, candy, coin + res0)
          case (Turn, Machine(false, candy, coin)) =>
            Machine(true, candy - res1, coin)
        }

      def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
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
