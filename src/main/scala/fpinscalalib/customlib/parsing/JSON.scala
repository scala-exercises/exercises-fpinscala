/*
 *  scala-exercises - exercises-fpinscala
 *  Copyright (C) 2015-2019 47 Degrees, LLC. <http://www.47deg.com>
 *
 */

package fpinscalalib.customlib.parsing

// The following implementation of the JSON Parser is provided by Manning as a solution to exercises found in the
// "Functional Programming in Scala" book. We'll use it in our section related to chapter 9: "Parser combinators".
// The original code can be found in the following URL:
//
// https://github.com/fpinscala/fpinscala/blob/44e01a7b5cc68cd681f182274b3a605db1bcff6c/answers/src/main/scala/fpinscala/parsing/JSON.scala

import language.higherKinds
import language.implicitConversions

trait JSON

object JSON {
  case object JNull                          extends JSON
  case class JNumber(get: Double)            extends JSON
  case class JString(get: String)            extends JSON
  case class JBool(get: Boolean)             extends JSON
  case class JArray(get: IndexedSeq[JSON])   extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    // we'll hide the string implicit conversion and promote strings to tokens instead
    // this is a bit nicer than having to write token everywhere
    import P.{string => _, _}

    implicit def tok(s: String) = P.token(P.string(s))

    def array  = surround("[", "]")(value sep "," map (vs => JArray(vs.toIndexedSeq))) scope "array"
    def obj    = surround("{", "}")(keyval sep "," map (kvs => JObject(kvs.toMap))) scope "object"
    def keyval = escapedQuoted ** (":" *> value)
    def lit = scope("literal") {
      "null".as(JNull) |
        double.map(JNumber(_)) |
        escapedQuoted.map(JString(_)) |
        "true".as(JBool(true)) |
        "false".as(JBool(false))
    }
    def value: Parser[JSON] = lit | obj | array
    root(whitespace *> (obj | array))
  }
}

/**
 * JSON parsing example.
 */
object JSONExample extends App {
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
                """

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
                       """

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
                       """

  val P = fpinscalalib.customlib.parsing.Reference
  import fpinscalalib.customlib.parsing.ReferenceTypes.Parser

  def printResult[E](e: Either[E, JSON]) =
    e.fold(println, println)

  val json: Parser[JSON] = JSON.jsonParser(P)
  printResult { P.run(json)(jsonTxt) }
  println("--")
  printResult { P.run(json)(malformedJson1) }
  println("--")
  printResult { P.run(json)(malformedJson2) }
}
