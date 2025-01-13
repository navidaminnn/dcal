package distcompiler.calc

import distcompiler.*
import Builtin.{Error, SourceMarker}

class CalcReaderTests extends munit.FunSuite:
  extension (str: String)
    def parse: Node.Top =
      calc.parse.fromSourceRange(SourceRange.entire(Source.fromString(str)))

    def evaluate: Node.Top =
      val top = parse
      CalcParser(top)
      top

  test("empty string"):
    assertEquals("".parse, Node.Top())

  test("only whitespace"):
    assertEquals("    \n\t".parse, Node.Top())

  test("error: invalid character"):
    assertEquals("k".parse, Node.Top(Error("invalid byte", SourceMarker("k"))))

  test("single number"):
    assertEquals(
      "5".parse, 
      Node.Top(
        tokens.Number("5")
      )
    )

  test("simple addition"):
    assertEquals(
      "5 + 11".parse, 
      Node.Top(
        tokens.Number("5"),
        tokens.Operator("+"),
        tokens.Number("11")
      )
    )

  test("addition calculation"):
    assertEquals("5 + 11".evaluate, Node.Top(tokens.Number("16")))

  // test("calculation"):
  //   assertEquals("10 - 7 + 4".evaluate, 7)

  // test("error: incorrect format"):
  //   intercept[IllegalArgumentException] (
  //     "10 - 10 7".evaluate
  //   )

  // test("multiplication calculation"):
  //   assertEquals("12 * 5 / 2".evaluate, 30)

  // test("error: divide by zero"):
  //   intercept[ArithmeticException] (
  //     "16 * 2 / 0".evaluate
  //   )

  // test("error: invalid character error"):
  //   intercept[IllegalArgumentException] (
  //     "5k * 10".evaluate
  //   )

  // test("full calculation"):
  //   assertEquals("10 + 7 * 5 - 9 / 3".evaluate, 42)