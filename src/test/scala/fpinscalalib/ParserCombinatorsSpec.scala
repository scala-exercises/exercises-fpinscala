package fpinscalalib

import org.scalatest.Spec
import org.scalatest.prop.Checkers
import org.scalacheck.Shapeless._
import org.scalaexercises.Test
import shapeless.HNil

class ParserCombinatorsSpec extends Spec with Checkers {
  def `parser many1 asserts` = {
    ParserCombinatorsSection.parserMany1Assert("a", "aaa")
  }

  def `parser many asserts` = {
    ParserCombinatorsSection.parserManyAssert(Right(List()), Right(List('a', 'a', 'a')))
  }

  def `parser listOfN asserts` = {
    ParserCombinatorsSection.parserListOfNAssert(0, 1)
  }

  def `parser flatMap asserts` = {
    ParserCombinatorsSection.parseFlatMapAssert("[0-9]+")
  }
}