package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class TestDCalTokenizer extends AnyFunSuite {
  import DCalTokenizer.*

  def pos(line: Int, column: Int): Position =
    Position(fileName = "<testfile>", line = line, column = column)

  List(
    "" -> Nil,
    "abc" -> List(
      ('a', pos(1, 1)),
      ('b', pos(1, 2)),
      ('c', pos(1, 3)),
    ),
    "a\nb" -> List(
      ('a', pos(1, 1)),
      ('\n', pos(2, 0)),
      ('b', pos(2, 1)),
    ),
    "a\nb\ncd" -> List(
      ('a', pos(1, 1)),
      ('\n', pos(2, 0)),
      ('b', pos(2, 1)),
      ('\n', pos(3, 0)),
      ('c', pos(3, 1)),
      ('d', pos(3, 2)),
    ),
  ).foreach {
    case (input, expectedResult) =>
      test(s"annotateCharactersWithPosition($input)") {
        val actualResult = annotateCharactersWithPosition(
          chars = input.view,
          fileName = "<testfile>",
        )
          .toList

        assert(actualResult == expectedResult)
      }
  }

  List(
    "12 " -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = TokenData.IntLiteral(BigInt(12))
      ),
    ),
  ).foreach {
    case (input, expectedResult) =>
      test(s"tokenize($input)") {
        val actualResult = tokenize(annotateCharactersWithPosition(
          chars = input.view,
          fileName = "<testfile>",
        ))
        assert(actualResult == expectedResult)
      }
  }
}
