package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class TestDCalTokenizer extends AnyFunSuite {
  import DCalTokenizer.*

  def pos(line: Int, column: Int): Position =
    Position(fileName = "<testfile>", line = line, column = column)

  List(
    "12" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = TokenData.IntLiteral(BigInt(12))
      ),
    ),
    "12 " -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = TokenData.IntLiteral(BigInt(12))
      ),
    ),
    "12\n13" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = TokenData.IntLiteral(BigInt(12))
      ),
      Token(
        startPosition = pos(2, 1),
        endPosition = pos(2, 2),
        data = TokenData.IntLiteral(BigInt(13))
      ),
    ),
  ).foreach {
    case (input, expectedResult) =>
      test(s"tokenize($input)") {
        val actualResult = tokenize(
          chars = input,
          fileName = "<testfile>",
        )
          .toList
        assert(actualResult == expectedResult)
      }
  }
}
