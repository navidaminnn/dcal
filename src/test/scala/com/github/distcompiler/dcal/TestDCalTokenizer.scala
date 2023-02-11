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
    """"p"""" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, """"p"""".length),
        data = TokenData.StringLiteral("p")
      )
    ),
    """"one string"""" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, """"one string"""".length),
        data = TokenData.StringLiteral("one string")
      )
    ),
    """"first line\nsecond line"""" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, """"first line\nsecond line"""".length),
        data = TokenData.StringLiteral("first line\nsecond line")
      )
    ),
    "_pv1" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, "_pv1".length),
        data = TokenData.Name("_pv1")
      )
    ),
    "let x = 19" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 3),
        data = TokenData.Let
      ),
      Token(
        startPosition = pos(1, 5),
        endPosition = pos(1, 5),
        data = TokenData.Name("x")
      ),
      Token(
        startPosition = pos(1, 7),
        endPosition = pos(1, 7),
        data = TokenData.Equals
      ),
      Token(
        startPosition = pos(1, 9),
        endPosition = pos(1, 10),
        data = TokenData.IntLiteral(19)
      )
    ),
    "var i \\in lst" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 3),
        data = TokenData.Var
      ),
      Token(
        startPosition = pos(1, 5),
        endPosition = pos(1, 5),
        data = TokenData.Name("i")
      ),
      Token(
        startPosition = pos(1, 7),
        endPosition = pos(1, 9),
        data = TokenData.SlashIn
      ),
      Token(
        startPosition = pos(1, 11),
        endPosition = pos(1, 13),
        data = TokenData.Name("lst")
      )
    ),
    """y := "new_y"""" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 1),
        data = TokenData.Name("y")
      ),
      Token(
        startPosition = pos(1, 3),
        endPosition = pos(1, 4),
        data = TokenData.Walrus
      ),
      Token(
        startPosition = pos(1, 6),
        endPosition = pos(1, 12),
        data = TokenData.StringLiteral("new_y")
      )
    ),
    "z1 := z2 || z2 := z1" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 2),
        data = TokenData.Name("z1")
      ),
      Token(
        startPosition = pos(1, 4),
        endPosition = pos(1, 5),
        data = TokenData.Walrus
      ),
      Token(
        startPosition = pos(1, 7),
        endPosition = pos(1, 8),
        data = TokenData.Name("z2")
      ),
      Token(
        startPosition = pos(1, 10),
        endPosition = pos(1, 11),
        data = TokenData.DoublePipe
      ),
      Token(
        startPosition = pos(1, 13),
        endPosition = pos(1, 14),
        data = TokenData.Name("z2")
      ),
      Token(
        startPosition = pos(1, 16),
        endPosition = pos(1, 17),
        data = TokenData.Walrus
      ),
      Token(
        startPosition = pos(1, 19),
        endPosition = pos(1, 20),
        data = TokenData.Name("z1")
      ),
    ),
    "def wait () {await lockRelease}" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 3),
        data = TokenData.Def
      ),
      Token(
        startPosition = pos(1, 5),
        endPosition = pos(1, 8),
        data = TokenData.Name("wait")
      ),
      Token(
        startPosition = pos(1, 10),
        endPosition = pos(1, 10),
        data = TokenData.OpenParenthesis
      ),
      Token(
        startPosition = pos(1, 11),
        endPosition = pos(1, 11),
        data = TokenData.CloseParenthesis
      ),
      Token(
        startPosition = pos(1, 13),
        endPosition = pos(1, 13),
        data = TokenData.OpenCurlyBracket
      ),
      Token(
        startPosition = pos(1, 14),
        endPosition = pos(1, 18),
        data = TokenData.Await
      ),
      Token(
        startPosition = pos(1, 20),
        endPosition = pos(1, 30),
        data = TokenData.Name("lockRelease")
      ),
      Token(
        startPosition = pos(1, 31),
        endPosition = pos(1, 31),
        data = TokenData.CloseCurlyBracket
      )
    ),
    "import ModuleA, ModuleB, ModuleC" -> List(
      Token(
        startPosition = pos(1, 1),
        endPosition = pos(1, 6),
        data = TokenData.Import
      ),
      Token(
        startPosition = pos(1, 8),
        endPosition = pos(1, 14),
        data = TokenData.Name("ModuleA")
      ),
      Token(
        startPosition = pos(1, 15),
        endPosition = pos(1, 15),
        data = TokenData.Comma
      ),
      Token(
        startPosition = pos(1, 17),
        endPosition = pos(1, 23),
        data = TokenData.Name("ModuleB")
      ),
      Token(
        startPosition = pos(1, 24),
        endPosition = pos(1, 24),
        data = TokenData.Comma
      ),
      Token(
        startPosition = pos(1, 26),
        endPosition = pos(1, 32),
        data = TokenData.Name("ModuleC")
      )
    )
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
