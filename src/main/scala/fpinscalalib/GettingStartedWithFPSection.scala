package fpinscalalib

import org.scalatest.{FlatSpec, Matchers}

/** @param name getting_started_with_functional_programming
  */
object GettingStartedWithFPSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {

  /**
    * = Tail-recursive functions =
    *
    * We're going to introduce some of the basic techniques for how to write functional programs. Let's start by writing
    * loops using tail-recursive functions. For instance, let's take a look on how to functionally write a function that
    * calculates the factorial of a given number.
    *
    * {{{
    * def factorial(n: Int): Int = {
    * @annotation.tailrec
    * def go(n: Int, acc: Int): Int =
    *   if (n <= 0) acc
    *   else go(n - 1, n * acc)
    *   go(n, 1)
    * }
    * }}}
    *
    * We're defining a recursive helper function inside the body of the `functional` function. We often call these helper
    * functions `go` or `loop`. Since it's local, the `go` function can only be referred to from within the body of the
    * `factorial` function, just like a local variable would.
    *
    * The arguments to `go` are the state for the loop. In this case, they're the remaining value `n`, and the current
    * accumulated factorial `acc`. To advance to the next iteration, we simply call `go` recursively with the new loop
    * state: `go(n-1, n*acc)`, and to exit from the loop we return a value without a recursive call (in this case, we
    * return the value of `acc` if `n <= 0`).
    *
    * Scala is able to detect this sort of self-recursion and compiles it to the same sort of bytecode as would be emitted
    * by a `while` loop, as long as the recursive call is in tail position. The basic idea is that this optimization
    * (tail call elimination) is applied when there's no additional work left to do after the recursive call returns.
    *
    * Let's do the same with a function to call the nth number from the Fibonacci sequence. The first two numbers
    * are 0 and 1. Then, the nth number is always the sum of the previous two, i.e.: 0, 1, 1, 2, 3, 5... The fib`
    * function starts by calling its `loop` helper function with the initial values of `n` (the position in the sequence
    * we need to calculate), and the previous and current values in the sequence.
    *
    * {{{
    *   def fib(n: Int): Int = {
    *   @annotation.tailrec
    *   def loop(n: Int, prev: Int, cur: Int): Int =
    *     if (n <= ???) prev
    *     else loop(n - ???, cur, prev + cur)
    *   loop(n, 0, 1)
    * }
    * }}}
    *
    * Try to fix the `loop` function inside `fib` so that it returns the correct values for each case in a tail-recursive
    * way. What should the missing expressions for the trivial case and the recursive call be?
    */

  def fibAssert(res0: Int, res1: Int) {
    def fib(n: Int): Int = {
      @annotation.tailrec
      def loop(n: Int, prev: Int, cur: Int): Int =
        if (n <= res0) prev
        else loop(n - res1, cur, prev + cur)
      loop(n, 0, 1)
    }

    fib(5) should be(5)
  }

  /**
    * = Polymorphic and higher-order functions =
    *
    * Polymorphic functions allow us to write code that works for any type it's given. For instance, take a look at
    * `findFirst`, a function that finds the first index in an array where the key occurs (or `-1` if it doesn't exist),
    * implemented more generally by accepting a function to use for testing a particular `A` value.
    *
    * {{{
    *   def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    *     @annotation.tailrec
    *     def loop(n: Int): Int =
    *       if (n >= as.length) -1
    *       // If the function `p` matches the current element,
    *       // we've found a match and we return its index in the array.
    *       else if (p(as(n))) n
    *       else loop(n + 1)
    *
    *     loop(0)
    *   }
    * }}}
    *
    * To write a polymorphic function as a method, we introduce a comma-separated list of type parameters, surrounded by
    * square brackets (here, just a single `[A]`), following the name of the function, in this case `findFirst`.
    *
    * = Higher-order functions =
    *
    * Let's see an example of a higher-order function (HOF):
    * {{{
    *   def formatResult(name: String, n: Int, f: Int => Int) = {
    *     val msg = "The %s of %d is %d."
    *     msg.format(name, n, f(n))
    *   }
    * }}}
    *
    * Our `formatResult` HOF takes another function called `f`. We give a type to `f`, as we would for any other parameter.
    * Its type is `Int => Int`, which indicates that `f` expects an integer argument and will also return an integer.
    *
    * Let's create a polymorphic, tail-recursive higher-order function that checks if an array is sorted, according to
    * a given comparison function that will be passed as a parameter:
    *
    * {{{
    *   def isSorted[A](as: Array[A], ordering: (A, A) => Boolean): Boolean = {
    *     @annotation.tailrec
    *     def go(n: Int): Boolean =
    *       if (n >= as.length - 1) true
    *       else if (ordering(as(n), as(n + 1))) false
    *       else go(n + 1)
    *
    *     go(0)
    *    }
    * }}}
    *
    * When using HOFs, it's often convenient to be able to call these functions with anonymous functions, rather than
    * having to supply some existing named function. For instance, using the previously implemented `findFirst`:
    *
    * {{{
    *   findFirst(Array(7, 9, 13), (x: Int) => x == 9)
    * }}}
    *
    * The syntax `(x: Int) => x == 9` is a `function literal` or `anonymous function`, defining a function that takes one
    * argument `x` of type `Int` and returns a `Boolean` indicating whether `x` is equal to 9.
    *
    * Let's do the same with `isSorted`. After taking a detailed look at its implementation, what would be the results of
    * applying the following anonymous functions to it?
    */

  def isSortedAssert(res0: Boolean, res1: Boolean, res2: Boolean): Unit = {
    def isSorted[A](as: Array[A], ordering: (A, A) => Boolean): Boolean = {
      @annotation.tailrec
      def go(n: Int): Boolean =
        if (n >= as.length - 1) true
        else if (ordering(as(n), as(n + 1))) false
        else go(n + 1)

      go(0)
    }

    isSorted(Array(1, 3, 5, 7), (x: Int, y: Int) => x > y) shouldBe res0
    isSorted(Array(7, 5, 3, 1), (x: Int, y: Int) => x < y) shouldBe res1
    isSorted(Array("Scala", "Exercises"), (x: String, y: String) => x.length > y.length) shouldBe res2
  }
}

