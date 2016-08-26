package fpinscalalib

import org.scalatest.{FlatSpec, Matchers}
import fpinscalalib.customlib._
import fpinscalalib.customlib.functionaldatastructures._
import fpinscalalib.customlib.functionaldatastructures.List._
import Tree._

/** @param name functional_data_structures
  */
object FunctionalDataStructuresSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {

  /**
    * = Singly linked lists =
    *
    * Let's examine what's probably the most ubiquitous functional data structure, the singly-linked list to start. The
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
    * the `trait` keyword. A `trait` is an abstract interface that may optionally contain implementations of some
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
    * is the first element, `x`, plus the sum of the remaining elements, `xs`. Likewise, the `product` definition states
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
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    x shouldBe res0
  }

  /**
    * Let's try implementing a few different functions for modifying lists in different ways. First, we'll implement the
    * function `tail` for removing the first element of a `List`. Also take a look at how we handle the use of `take` on
    * `Nil` lists:
    *
    * {{{
    *   def tail[A](l: List[A]): List[A] =
    *     l match {
    *     case Nil => sys.error("tail of empty list")
    *     case Cons(_,t) => t
    *     }
    * }}}
    *
    * After taking a look at its implementation, check out its behaviour on the following cases:
    */

  def listTakeAssert(res0: List[Int], res1: List[Int]) {
    tail(List(1, 2, 3)) shouldBe res0
    tail(List(1)) shouldBe res1
  }

  /**
    * Using the same idea, we can implement the function `setHead` for replacing the first element of a List with a
    * different value:
    *
    * {{{
    *   def setHead[A](l: List[A], h: A): List[A] = l match {
    *     case Nil => sys.error("setHead on empty list")
    *     case Cons(_,t) => Cons(h,t)
    *   }
    * }}}
    *
    * Let's see how it behaves:
    */

  def listSetHeadAssert(res0: List[Int], res1: List[String]) {
    setHead(List(1, 2, 3), 3) shouldBe res0
    setHead(List("a", "b"), "c") shouldBe res1
  }

  /**
    * We can generalize `take` to the function `drop`, which removes the first `n` elements from a list.
    *
    * {{{
    *   def drop[A](l: List[A], n: Int): List[A] =
    *     if (n <= 0) l
    *     else l match {
    *       case Nil => Nil
    *       case Cons(_,t) => drop(t, n-1)
    *     }
    * }}}
    *
    * Again, we can see its behaviour with the following exercises:
    */

  def listDropAssert(res0: List[Int], res1: List[Int], res2: List[Int], res3: List[Int], res4: List[Int]) {
    drop(List(1, 2, 3), 1) shouldBe res0
    drop(List(1, 2, 3), 0) shouldBe res1
    drop(List("a", "b"), 2) shouldBe res2
    drop(List(1, 2), 3) shouldBe res3
    drop(Nil, 1) shouldBe res4
  }

  /**
    * `dropWhile` extends the behaviour of `drop`, removing elements from the `List` prefix as long as they match a
    * predicate
    *
    * {{{
    *   def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    *     l match {
    *       case Cons(h,t) if f(h) => dropWhile(t, f)
    *       case _ => l
    *     }
    * }}}
    *
    * See how it works with the following examples:
    */

  def listDropWhileAssert(res0: List[Int], res1: List[Int], res2: List[Int], res3: List[Int]) {
    dropWhile(List(1, 2, 3), (x: Int) => x < 2) shouldBe res0
    dropWhile(List(1, 2, 3), (x: Int) => x > 2) shouldBe res1
    dropWhile(List(1, 2, 3), (x: Int) => x > 0) shouldBe res2
    dropWhile(Nil, (x: Int) => x > 0) shouldBe res3
  }

  /**
    * In the same fashion, let's implement another function `init` that returns a `List` consisting of all but the last
    * element of a `List`. It should be noted that this function cannot be implemented in constant time like `tail`.
    *
    * {{{
    *   def init[A](l: List[A]): List[A] =
    *     l match {
    *       case Nil => sys.error("init of empty list")
    *       case Cons(_,Nil) => Nil
    *       case Cons(h,t) => Cons(h,init(t))
    *     }
    * }}}
    *
    * Let's look at how it works:
    */

  def listInitAssert(res0: List[Int], res1: List[Int]) {
    init(List(1, 2, 3)) shouldBe res0
    init(List(1)) shouldBe res1
  }

  /**
    * = Recursion over lists and generalizing to higher-order functions =
    *
    * Once again, let’s take a look at the implementations of `sum` and `product`. We’ve simplified the product
    * implementation slightly, so as not to include the “short-circuiting” logic of checking for 0.0:
    *
    * {{{
    * def sum(ints: List[Int]): Int = ints match {
    *   case Nil => 0
    *   case Cons(x,xs) => x + sum(xs)
    *   }
    *
    * def product(ds: List[Double]): Double = ds match {
    *   case Nil => 1.0
    *   case Cons(x, xs) => x * product(xs)
    *   }
    * }}}
    *
    * Note how similar these two definitions are. They’re operating on different types (List[Int] versus List[Double]),
    * but aside from this, the only differences are the value to return in the case that the list is empty (0 in the case
    * of sum, 1.0 in the case of product), and the operation to combine results (+ in the case of sum, * in the case of
    * product).
    *
    * We can improve things by generalizing those functions, by implementing a `foldRight`. This function will take on as
    * arguments the value to return in the case of the empty list, and the function to add an element to the result in
    * the case of a nonempty list:
    *
    * {{{
    * def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    *   as match {
    *     case Nil => z
    *     case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    *   }
    *
    * def sum2(ns: List[Int]) = foldRight(ns, 0)((x,y) => x + y)
    * def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
    * }}}
    *
    * `foldRight` replaces the constructors of the list, `Nil` and `Cons`, with `z` and `f`, illustrated here:
    *
    * {{{
    *   Cons(1, Cons(2, Nil))
    *   f   (1, f   (2, z  ))
    * }}}
    *
    * Let's run through the steps that `foldRight` will follow in our new implementation of `sum2`:
    *
    */

  def listFoldRightSumAssert(res0: Int,res1: Int, res2: Int, res3: Int, res4: Int, res5: Int, res6: List[Int], res7: Int, res8: Int, res9: Int, res10: Int) {
    foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)((x,y) => x + y) shouldBe 6
    res0 + foldRight(Cons(2, Cons(3, Nil)), 0)((x,y) => x + y) shouldBe 6
    res1 + res2 + foldRight(Cons(3, Nil), 0)((x,y) => x + y) shouldBe 6
    res3 + res4 + res5 + foldRight(res6, 0)((x,y) => x + y) shouldBe 6
    res7 + res8 + res9 + res10 shouldBe 6
  }

  /**
    * Now that we know how `foldRight` works, try to think about what happens when you pass `Nil` and `Cons` themselves
    * to `foldRight`.
    */

  def listFoldRightNilConsAssert(res0: List[Int]) {
    foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)) shouldBe res0
  }

  /**
    * Let's try to use `foldRight` to calculate the length of a list. Try to fill the gaps in our implementation:
    */

  def listLengthAssert(res0: Int, res1: Int): Unit = {
    def l = List(1, 2, 3, 4, 5)
    def length[A](as: List[A]): Int = List.foldRight(l, res0)((_, acc) => acc + res1)

    length(l) shouldBe 5
  }

  /**
    * Our implementation of `foldRight` is not tail-recursive and will result in a `StackOverflowError` for large lists
    * (we say it's not `stack-safe`). Let's write another general list-recursion function, `foldLeft`, that is
    * tail-recursive, using the techniques we discussed in the previous chapter:
    *
    * {{{
    *   def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    *     l match {
    *       case Nil => z
    *       case Cons(h,t) => foldLeft(t, f(z,h))(f)
    *     }
    * }}}
    *
    * Let's write functions `sum`, `product` and `length` of a list using `foldLeft`:
    */

  def listFoldLeftSumProductLengthAssert(res0: Int, res1: Double, res2: Int, res3: Int): Unit = {
    def sum3(l: List[Int]) = foldLeft(l, res0)(_ + _)
    def product3(l: List[Double]) = foldLeft(l, res1)(_ * _)
    def length2[A](l: List[A]): Int = foldLeft(l, res2)((acc,h) => acc + res3)

    def listInts = List(1, 2, 3, 4, 5)
    def listDoubles = List(1.0, 2.0, 3.0)
    sum3(listInts) shouldBe 15
    product3(listDoubles) shouldBe 6.0
    length2(listInts) shouldBe 5
  }

  /**
    * As we saw above, we can write the previous functions we implemented using `foldRight` with `foldLeft`. Let's continue
    * with `reverse`:
    *
    * {{{
    *   def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
    * }}}
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
    * The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows
    * when implementing a strict `foldRight` function as we've done in this chapter. Note that the other implementation is
    * more of theoretical interest - it isn't stack-safe and won't work for large lists.
    *
    */

  /**
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
    */

  /**
    * Let's keep digging into the uses of `foldLeft` and `foldRight`, by implementing a function that transforms a list
    * of integers by adding 1 to each element:
    */

  def listAdd1Assert(res0: Int): Unit = {
    def add1(l: List[Int]): List[Int] = foldRight(l, Nil : List[Int])((h, t) => Cons(h + res0,t))
    add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  /**
    * We can do something similar to turn each value in a List[Double] into a String:
    *
    * {{{
    *   def doubleToString(l: List[Double]): List[String] =
    *     foldRight(l, Nil:List[String])((h,t) => Cons(h.toString,t))
    * }}}
    */

  /**
    * Both `add1` and `doubleToString` modify each element in a list while maintaining its structure. We can generalize
    * it in the following way:
    *
    * {{{
    *   def map[A,B](l: List[A])(f: A => B): List[B] =
    *     foldRight(l, Nil:List[B])((h,t) => Cons(f(h),t))
    * }}}
    *
    * You may notice that we are using `foldRight` to implement `map`, even though it's not stack-safe. We can also
    * use `foldRightViaFoldLeft` (that relies on reversing the original list), or use local mutation. Take a look at both
    * alternatives:
    *
    * {{{
    *   def map_1[A,B](l: List[A])(f: A => B): List[B] =
    *     foldRightViaFoldLeft(l, Nil:List[B])((h,t) => Cons(f(h),t))
    *
    *   def map_2[A,B](l: List[A])(f: A => B): List[B] = {
    *     val buf = new collection.mutable.ListBuffer[B]
    *     def go(l: List[A]): Unit = l match {
    *       case Nil => ()
    *       case Cons(h,t) => buf += f(h); go(t)
    *     }
    *     go(l)
    *     List(buf.toList: _*) // converting from the standard Scala list to the list we've defined here
    *   }
    * }}}
    */

  /**
    * Let's apply the same principle as we use in `map` to remove elements from a list, starting with a function to
    * remove all odd numbers from a List[Int]:
    */
  def listRemoveOdds(res0: Int, res1: Int): Unit = {
    def removeOdds(l: List[Int]): List[Int] =
      foldRight(l, Nil:List[Int])((h, t) => if (h % res0 == res1) Cons(h, t) else t)
    removeOdds(List(1, 2, 3, 4, 5)) shouldBe List(2, 4)
  }

  /**
    * Following the same principle, let's generalize the function above to be able to remove elements from a list unless
    * they satisfy a given predicate:
    *
    * {{{
    *   def filter[A](l: List[A])(f: A => Boolean): List[A] =
    *     foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)
    * }}}
    *
    * The same considerations regarding the different choices of implementations as in `map` apply to `filter`. The one
    * above isn't stack-safe, so we should make a choice between using `foldRightViaFoldLeft` instead of `foldRight`, or
    * perform local mutations.
    */

  /**
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

  def listFlatMapAssert(res0: List[Int]): Unit = {
    flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe res0
  }

  /**
    * We can also implement `filter` using `flatMap`:
    *
    * {{{
    *   def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    *     flatMap(l)(a => if (f(a)) List(a) else Nil)
    * }}}
    *
    */

  /**
    * Now we're going to write a function that accepts two lists of integers and constructs a new list by adding
    * corresponding elements. For example, `List(1, 2, 3)` and `List(4, 5, 6)` become `List(5, 7, 9)`:
    *
    * {{{
    *   def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    *     case (Nil, _) => Nil
    *     case (_, Nil) => Nil
    *     case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
    *   }
    * }}}
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
    * List is just one example of what’s called an algebraic data type (ADT). An ADT is just a data type defined by one
    * or more data constructors, each of which may contain zero or more arguments. We say that the data type is the sum
    * or union of its data constructors, and each data constructor is the product of its arguments, hence the name
    * algebraic data type.
    *
    * Algebraic data types can be used to define other data structures. Let’s define a simple binary tree data structure:
    *
    * {{{
    *   sealed trait Tree[+A]
    *   case class Leaf[A](value: A) extends Tree[A]
    *   case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    * }}}
    *
    * Pattern matching again provides a convenient way of operating over elements of our ADT. Let’s try writing a few
    * functions. For starters, let's try to implement a function `size` to count the number of nodes (leaves and branches)
    * in a tree:
    */

  def treeSizeAssert(res0: Int, res1: Int): Unit = {
    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => res0
      case Branch(l, r) => res1 + size(l) + size(r)
    }

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    size(t) shouldBe 5
  }

  /**
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
    * In the same fashion, let's implement a function `depth` that returns the maximum path length from the root of a
    * tree to any leaf.
    */

  def treeDepthAssert(res0: Int, res1: Int): Unit = {
    def depth[A](t: Tree[A]): Int = t match {
      case Leaf(_) => res0
      case Branch(l,r) => res1 + (depth(l) max depth(r))
    }
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    depth(t) shouldBe 2
  }

  /**
    * We can also write a function `map`, analogous to the method of the same name on `List`, that modifies each element
    * in a tree with a given function:
    *
    * {{{
    *   def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    *     case Leaf(a) => Leaf(f(a))
    *     case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    *   }
    * }}}
    *
    * Let's try it out in the following exercise:
    */

  def treeMapAssert(res0: Branch[Int]): Unit = {
    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    Tree.map(t)(_ * 2) shouldBe res0
  }

  /**
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
      fold(t)(a => res2)((d1,d2) => res3 + (d1 max d2))

    def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
      fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))

    def t = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    sizeViaFold(t) shouldBe 5
    maximumViaFold(t) shouldBe 3
    depthViaFold(t) shouldBe 2
    mapViaFold(t)(_ % 2 == 0) shouldBe res4
  }
}

