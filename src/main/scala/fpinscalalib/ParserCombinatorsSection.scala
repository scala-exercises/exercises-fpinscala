package fpinscalalib

import org.scalatest.{FlatSpec, Matchers}
import fpinscalalib.customlib.parsing.{JSON, Location, ParseError, Reference}
import fpinscalalib.customlib.parsing.ReferenceTypes._
import Reference._
import scala.util.matching.Regex

/** @param name parser_combinators
  */
object ParserCombinatorsSection extends FlatSpec with Matchers with org.scalaexercises.definitions.Section with ReferenceHelper {

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
    */
  def parserMany1Assert(res0: String, res1: String): Unit = {
    val parser = run(
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

    val parser = run(
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
    val parseFunction = run(listParser)(_)

    parseFunction("aaa") shouldBe Right(List('a', 'a', 'a'))
    parseFunction("a").isLeft shouldBe true
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
      digit <- Reference.regex(res0.r)
      n = digit.toInt
      _ <- listOfN(n, char('a'))
    } yield n

    val parseFunction = run(parser)(_)
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
    * <b>Exercise 9.9</b>
    *
    * Let's see an example of how a `Parser[JSON]` could be implemented using the primitives we've defined:
    *
    * {{{
    *   trait JSON
    *
    *   object JSON {
    *     case object JNull extends JSON
    *     case class JNumber(get: Double) extends JSON
    *     case class JString(get: String) extends JSON
    *     case class JBool(get: Boolean) extends JSON
    *     case class JArray(get: IndexedSeq[JSON]) extends JSON
    *     case class JObject(get: Map[String, JSON]) extends JSON
    *
    *     def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    *       import P.{string => _, _}
    *       implicit def tok(s: String) = P.token(P.string(s))
    *
    *       def array = surround("[","]")(
    *         value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"
    *
    *       def obj = surround("{","}")(
    *         keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"
    *
    *       def keyval = escapedQuoted ** (":" *> value)
    *
    *       def lit = scope("literal") {
    *         "null".as(JNull) |
    *         double.map(JNumber(_)) |
    *         escapedQuoted.map(JString(_)) |
    *         "true".as(JBool(true)) |
    *         "false".as(JBool(false))
    *       }
    *
    *       def value: Parser[JSON] = lit | obj | array
    *         root(whitespace *> (obj | array))
    *     }
    *   }
    * }}}
    *
    * = Error reporting =
    *
    * <b>Exercise 9.11</b>
    *
    * Some useful primitives that could be useful to let programmers specify what error(s) get reported in an `or` chain
    * could be:
    *
    * {{{
    *   /** In the event of an error, returns the error that occurred after consuming the most number of characters. */
    *   def furthest[A](p: Parser[A]): Parser[A]
    *
    *   /** In the event of an error, returns the error that occurred most recently. */
    *   def latest[A](p: Parser[A]): Parser[A]
    * }}}
    *
    * = One possible implementation =
    *
    * <b>Explore 9.13</b>
    *
    * We'll be exploring an actual representation of `Parser`. Let's begin by implementing some of its methods, starting
    * with `string`:
    */

  def parserStringAssert(res0: String): Unit = {
    def string(w: String): Parser[String] = {
      val msg = "'" + w + "'"
      s => {
        val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
        if (i == -1) // they matched
          Success(w, w.length)
        else
          Failure(s.loc.advanceBy(i).toError(msg), i != 0)
      }
    }

    val parseFunction = run(
      string("42")
    )(_)

    parseFunction(res0) shouldBe Right("42")
  }

  /**
    * Let's continue by implementing `regex`:
    */

  def parserRegexAssert(res0: String): Unit = {
    def regex(r: Regex): Parser[String] = {
      val msg = "regex " + r
      s => r.findPrefixOf(s.input) match {
        case None => Failure(s.loc.toError(msg), false)
        case Some(m) => Success(m,m.length)
      }
    }

    val parserFunction = run(
      regex(res0.r)
    )(_)

    parserFunction("12345") shouldBe Right("12345")
    parserFunction("abcde").isLeft shouldBe true
  }

  /**
    * Implementation of `succeed` is pretty straightforward:
    *
    * {{{
    *   def succeed[A](a: A): Parser[A] = s => Success(a, 0)
    * }}}
    *
    * Finally let's implement `slice` for our new representation:
    */

  def parserSliceAssert(res0: Boolean): Unit = {
    def slice[A](p: Parser[A]): Parser[String] =
      s => p(s) match {
        case Success(_,n) => Success(s.slice(n),n)
        case f@Failure(_,_) => f
      }

    val parserFunction = run(
      slice(many1(string("a")))
    )(_)

    parserFunction("a") shouldBe Right("a")
    parserFunction("ab") shouldBe Right("a")
    parserFunction("b").isLeft shouldBe res0
  }

  /**
    * <b>Exercise 9.15</b>
    *
    * It's time to implement the `run` primitive:
    *
    * {{{
    *   def run[A](p: Parser[A])(s: String): Either[ParseError,A] = {
    *     val s0 = ParseState(Location(s))
    *     p(s0).extract
    *   }
    * }}}
    *
    * <b>Exercise 9.16</b>
    *
    * `ParseError` can be improved by better formatting it for human consumption. Let's see how this could work:
    *
    * {{{
    *   case class ParseError(stack: List[(Location,String)] = List()) {
    *     def push(loc: Location, msg: String): ParseError =
    *       copy(stack = (loc,msg) :: stack)
    *
    *     def label[A](s: String): ParseError =
    *       ParseError(latestLoc.map((_,s)).toList)
    *
    *     def latest: Option[(Location,String)] = stack.lastOption
    *
    *     def latestLoc: Option[Location] = latest map (_._1)
    *
    *     // Display collapsed error stack - any adjacent stack elements with the same location are combined on one line.
    *     // For the bottommost error, we display the full line, with a caret pointing to the column of the error.
    *     override def toString =
    *       if (stack.isEmpty) "no error message"
    *     else {
    *       val collapsed = collapseStack(stack)
    *       val context =
    *         collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
    *         collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
    *       collapsed.map { case (loc,msg) => loc.line.toString + "." + loc.col + " " + msg }.mkString("\n") + context
    *     }
    *
    *     // Builds a collapsed version of the given error stack - messages at the same location have their messages
    *     // merged, separated by semicolons.
    *     def collapseStack(s: List[(Location,String)]): List[(Location,String)] =
    *       s.groupBy(_._1).
    *       mapValues(_.map(_._2).mkString("; ")).
    *       toList.sortBy(_._1.offset)
    *
    *     def formatLoc(l: Location): String = l.line + "." + l.col
    * }}}
    *
    * <b>Exercise 9.17</b>
    *
    * In order to make the `slice` combinator more efficient (i.e.: `many(char('a')).slice` will still create a
    * `List[Char]`, only to discard it) we can create a different representation of the `Parser` type. The main change
    * is to add another piece of state to `ParseState`, an `isSliced` flag, and an additional `Slice` constructor to
    * `Result`. If the `isSliced` flag is set, parsers avoid building a meaningful result.
    * You can take a look at the complete implementation
    * <a href="https://github.com/fpinscala/fpinscala/blob/7c068281c0f534c5fe24b9d417a7ce7c193c2d0f/answers/src/main/scala/fpinscala/parsing/instances/Sliceable.scala">here</a>.
    *
    * <b>Exercise 9.18</b>
    *
    * Right now, we're missing error information when we combine several parsers with the `or` combinator. For instance,
    * when both parsers fail we only take into account errors from the second parser. Maybe we could show both error
    * messages, or choose the error from the branch that got furthest without failing. Let's sketch a way of doing that:
    *
    * {{{
    *   // We'll just give a sketch here. The basic idea is to add an additional field to `ParseError`
    *   case class ParseError(stack: List[(Location,String)] = List(),
    *                         otherFailures: List[ParseError] = List()) {
    *
    *     def addFailure(e: ParseError): ParseError =
    *       this.copy(otherFailures = e :: this.otherFailures)
    *       // ...
    *     }
    *
    *  // We then need to make sure we populate this in the implementation of `or`
    *     def or[A](p: Parser[A], p2: => Parser[A]): Parser[A] =
    *       s => p(s) match {
    *         case Failure(e,false) => p2(s).mapError(_.addFailure(e))
    *         case r => r // committed failure or success skips running `p2`
    *       }
    * }}}
    *
    * Of course, we have to decide how to print a `ParseError` for human consumption. We also can expose combinators for
    * selecting which error(s) get reported in the event that a chain of `a | b | c` fails--we might choose to collect up
    * all the errors for each of the three parsers, or perhaps only show the parser that got the furthest in the input
    * before failing, etc.
    *
    * <b>Exercise 9.x</b>
    *
    * To wrap this section up, here's a small exercise for you to test the aforementioned `JSON` parser with your own
    * entries. Source code for this parser can be found
    * <a href="https://raw.githubusercontent.com/fpinscala/fpinscala/44e01a7b5cc68cd681f182274b3a605db1bcff6c/answers/src/main/scala/fpinscala/parsing/JSON.scala">here</a>
    * or in the source code of this section.
    */

  def parserJSONAssert(res0: String): Unit = {
    val json: Parser[JSON] = JSON.jsonParser(Reference)
    run(json)(res0).isRight shouldBe true
  }
}

trait ReferenceHelper {
  def run[A](p: Parser[A])(s: String) = Reference.run(p)(s)
}