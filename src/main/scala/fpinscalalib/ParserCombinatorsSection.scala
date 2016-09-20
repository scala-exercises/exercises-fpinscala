package fpinscalalib

import org.scalatest.{FlatSpec, Matchers}
import fpinscalalib.customlib.parsing.{Location, ParseError, Reference}
import fpinscalalib.customlib.parsing.ReferenceTypes._

import scala.util.matching.Regex

/** @param name parser_combinators
  */
object ParserCombinatorsSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section {

  /**
    * = Slicing and nonempty repetition =
    *
    * <b>Exercise 9.1</b>
    *
    * First let's implement `map2` in our `Parser` type:
    *
    * {{{
    *   def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    *     map(product(p, p2))(f.tupled)
    * }}}
    *
    * With `map2` in our set of combinators, we can implement `many1`:
    *
    * {{{
    *   def many1[A](p: Parser[A]): Parser[List[A]] =
    *     map2(p, many(p))(_ :: _)
    * }}}
    *
    * You'll see during this chapter several references to an `object` named `Reference` (no pun intended!). It contains
    * an implementation of the functions contained in the `Parsers` trait which we'll be using heavily during the
    * exercises. We recommend to take an in-depth look at its source code to understand how to implement those methods.
    */

  import Reference._
  def parserMany1Assert[Parser[+_]](res0: String, res1: String): Unit = {
    val parser = Reference.run(
      many1(char('a'))
    )(_)

    parser(res0) shouldBe Right(List('a'))
    parser(res1) shouldBe Right(List('a', 'a', 'a'))
  }

  /**
    * <b>Exercise 9.2</b>
    *
    * Regarding laws that specify the behavior of `product` (`**`), let's start by stating that `product` is associative.
    * These two expressions are "roughly" equal:
    *
    * {{{
    *   (a ** b) ** c
    *   a ** (b ** c)
    * }}}
    *
    * The only difference is how the pairs are nested. The `(a ** b) ** c` parser returns an `((A,B), C)`, whereas the
    * `a ** (b ** c)` returns an `(A, (B,C))`. We can define functions `unbiasL` and `unbiasR` to convert these nested
    * tuples to flat 3-tuples:
    *
    * {{{
    *   def unbiasL[A,B,C](p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)
    *   def unbiasR[A,B,C](p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)
    * }}}
    *
    * With these, we can now state the associativity property:
    *
    * {{{
    *   (a ** b) ** c map (unbiasL) == a ** (b ** c) map (unbiasR)
    * }}}
    *
    * We'll sometimes just use `~=` when there is an obvious bijection between the two sides:
    *
    * {{{
    *   (a ** b) ** c ~= a ** (b ** c)
    * }}}
    *
    * `map` and `product` also have an interesting relationship--we can `map` either before or after taking the product
    * of two parsers, without affecting the behavior:
    *
    * {{{
    *   a.map(f) ** b.map(g) == (a ** b) map { case (a,b) => (f(a), g(b)) }
    * }}}
    *
    * For instance, if `a` and `b` were both `Parser[String]`, and `f` and `g` both computed the length of a string, it
    * doesn't matter if we map over the result of `a` to compute its length, or whether we do that after the product.
    *
    * For more discussion of these laws, take a look at chapter 12 of "Functional Programming in Scala".
    *
    * <b>Exercise 9.3</b>
    *
    * Let's try to define `many` via `or`, `map2`, and `succeed`:
    */

  def parserManyAssert(res0: Either[ParseError, List[Char]], res1: Either[ParseError, List[Char]]): Unit = {
    def many[A](p: Parser[A]): Parser[List[A]] =
      map2(p, many(p))(_ :: _) or succeed(List())

    val parser = Reference.run(
      many(char('a'))
    )(_)

    parser("") shouldBe res0
    parser("aaa") shouldBe res1
  }

  /**
    * <b>Exercise 9.4</b>
    *
    * Using `map2` and `succeed`, we can also implement `listOfN`:
    */

  def parserListOfNAssert(res0: Int, res1: Int): Unit = {
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= res0) succeed(List())
      else map2(p, listOfN(n - res1, p))(_ :: _)

    val singleParser = char('a')
    val listParser = listOfN(3, singleParser)
    val parseFunction = Reference.run(listParser)(_)

    parseFunction("aaa") shouldBe Right(List('a', 'a', 'a'))

    parseFunction("a") shouldBe Left(
      ParseError(List((Location("a", 1), "'a'")))
    )
  }

  /**
    * <b>Exercise 9.5</b>
    *
    * One way we can deal with non-strictness, is by using a new combinator (`wrap`):
    *
    * {{{
    *   def wrap[A](p: => Parser[A]): Parser[A]
    * }}}
    *
    * We can then define `many` as:
    *
    * {{{
    *   def many[A](p: Parser[A]): Parser[List[A]] =
    *     map2(p, wrap(many(p)))(_ :: _) or succeed(List())
    * }}}
    *
    * In the parallelism chapter, we were particularly interested in avoiding having `Par` objects that took as much time
    * and space to build as the corresponding serial computation, and the `delay` combinator let us control this more
    * carefully. Here, this isn't as much of a concern, and having to think carefully each time we `map2` to decide
    * whether we need to call `wrap` seems like unnecessary friction for users of the API.
    *
    * = Handling context sensitivity =
    *
    * <b>Exercise 9.6</b>
    *
    * We're now going to try parse a single digit (i.e.: `4`), followed by `that many` `'a'` characters. For instance:
    * `"0"`, `"1a"`, `"2aa"`, and so on. This is also called a context-sensitive grammar. It can't be expressed with
    * `product` because what the second parser decides depends on the first parser's result. Similarly as we did in
    * previous chapters, this problem calls for the use of a new primitive, `flatMap`:
    *
    * {{{
    *   def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
    * }}}
    *
    * To parse the digits, we'll be using regular expressions. Note the use of the new primitive `regex` which converts
    * any regular expression into a `Parser`. Complete the implementation with a regular expression to catch the digits
    * in our parser's first step, and take a look on how we chain both operations:
    */

  def parseFlatMapAssert(res0: String): Unit = {
    val parser = for {
      digit <- Reference.regex("[0-9]+".r)
      n = digit.toInt
      _ <- listOfN(n, char('a'))
    } yield n

    val parseFunction = Reference.run(parser)(_)
    parseFunction("0") shouldBe Right(0)
    parseFunction("1a") shouldBe Right(1)
    parseFunction("2aa") shouldBe Right(2)
  }

  /**
    * <b>Exercise 9.7</b>
    *
    * We can also implement `product` and `map2` in terms of `flatMap`:
    *
    * {{{
    *   def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    *     flatMap(p)(a => map(p2)(b => (a,b)))
    *
    *   def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    *     for { a <- p; b <- p2 } yield f(a,b)
    * }}}
    *
    * <b>Exercise 9.8</b>
    *
    * The same way as `map` can be implemented via `flatMap`:
    *
    * {{{
    *   def map[A,B](p: Parser[A])(f: A => B): Parser[B] = p.flatMap(a => succeed(f(a)))
    * }}}
    *
    */
}