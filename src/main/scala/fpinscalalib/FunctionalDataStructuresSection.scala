package fpinscalalib

import org.scalatest.{FlatSpec, Matchers}

/** @param name functional_data_structures
  */
object FunctionalDataStructuresSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {

  /**
    * = Singly linked lists =
    *
    * First let's examine what's probably the most ubiquitous functional data structure, the singly linked list. The
    * definition here is identical in spirit to (though simpler than) the `List` data type defined in Scala's standard
    * library.
    *
    * {{{
    * sealed trait List[+A]
    * case object Nil extends List[Nothing]
    *
    * object List {
    *   def sum(ints: List[Int]): Int = ints match {
    *     case Nil => 0
    *     case Cons(x,xs) => x + sum(xs)
    *   }
    *
    *   def product(ds: List[Double]): Double = ds match {
    *     case Nil => 1.0
    *     case Cons(0.0, _) => 0.0
    *     case Cons(x,xs) => x * product(xs)
    *   }
    *
    *   def apply[A](as: A*): List[A] =
    *     if (as.isEmpty) Nil
	  *     else Cons(as.head, apply(as.tail: _*))
    * }
    * }}}
    *
    * The definition of the data type begins with the keywords `sealed trait`. In general, we introduce a data type with
    * the `trait` keyword. A `trait` is a an abstract interface that may optionally contain implementations of some
    * methods. There are two such implementations, or data constructors, of `List`, to represent the two possible forms a
    * `List` can take. A `List` can be empty (denoted by the data constructor `Nil`), or it can be nonempty, denoted by
    * the data constructor `Cons` (traditionally short for `construct`). A nonempty list consists of an initial element,
    * `head`, followed by a (possibly empty) `List` of remaining elements (the `tail`).
    *
    * = Pattern matching =
    *
    * Each data constructor also introduces a `pattern` that can be used for `pattern matching`, as in the functions
    * `sum` and `product`. Both functions are defined in the `object` `List`, sometimes called the `companion object` to
    * `List`. Both definitions make use of pattern matching.
    *
    * As you might expect, the `sum` function states that the sum of an empty list is 0, and the sum of a nonempty list
    * is the first element, `x`, plus the sum of the remaining elements, `xs`. Likewise the `product` definition states
    * that the product of an empty list is `1.0`, the product of any other nonempty list starting with `0.0` is `0.0`,
    * and the product of any other nonempty list is the first element multiplied by the product of the remaining elements.
    *
    * Pattern matching works a bit like a fancy `switch` statement that may descend into the structure of the expression
    * it examines and extract subexpressions of that structure. Each case in the match consists of a `pattern` (like
    * `Cons(x, xs)`), and a `result` (like `x * product(xs)`). If the target `matches` the pattern in a case, the result
    * of that case becomes the result of the entire match expression. If multiple patterns match the target, Scala chooses
    * the first matching case. Let's look at some examples:
    */

  def patternMatching101Assert(res0: Int, res1: Int, res2: List[Int]) {
    (List(1, 2, 3) match { case _ => 42 }) shouldBe res0
    (List(1, 2, 3) match { case Cons(h, _) => h }) shouldBe res1
    (List(1, 2, 3) match { case Cons(_, t) => t }) shouldBe res2
  }

  /**
    * The following example is a little bit different:
    *
    * {{{
    *   List(1, 2, 3) match { case Nil => 42 }
    * }}}
    *
    * What happens if not a single case in a match expression matches the target? Scala will return a `MatchError` at
    * runtime.
    *
    * A pattern may contain `literals` (like `3` or `"hi"`), `variables` like `x` and `xs`, which match anything,
    * indicated by an identifier starting with a lowercase letter or underscore; and data constructors like `Cons(x, xs)`
    * and `Nil`, which match only values of the corresponding form. These components may be nested arbitrarily.
    *
    * Examine the next complex match expression. What will be the result?
    */

  def complexPatternAssert(res0: Int): Unit = {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    x shouldBe res0
  }
}

