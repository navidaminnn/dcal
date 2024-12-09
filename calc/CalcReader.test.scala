package distcompiler.calc

import distcompiler.*
import Builtin.{Error, SourceMarker}

class CalcReaderTests extends munit.FunSuite:
  extension (str: String)
    def evaluate: Int = CalcReader.evaluate(CalcReader.tokenize(str))

  // TODO:
    // support negative numbers
    // support brackets for precedence

  test("single number"):
    assertEquals("42".evaluate, 42)

  test("simple addition"):
    assertEquals("10 + 5".evaluate, 15)

  test("multi-digit subtraction"):
    assertEquals("5 - 3 - 2".evaluate, 0)

  test("multi-digit multiplication"):
    assertEquals("5 * 2 * 11".evaluate, 110)

  test("simple division"):
    assertEquals("10 / 2".evaluate, 5)

  test("multiplication + division"):
    assertEquals("10 * 5 / 2".evaluate, 25)

  test("invalid character error"):
    intercept[IllegalArgumentException] (
      "5k * 10".evaluate
    )

  test("divide by zero error"):
    intercept[ArithmeticException] (
      "5 / 0".evaluate
    )
