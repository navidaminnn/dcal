package distcompiler.calc

import distcompiler.*
import Builtin.{Error, SourceMarker}

class CalcReaderTests extends munit.FunSuite:
  extension (str: String)
    def read: Node.Top =
      calc.read.fromSourceRange(SourceRange.entire(Source.fromString(str)))

    def parse: Node.Top = 
      val top = read

      CalcParser(top, tracer = Manip.RewriteDebugTracer(os.pwd / "dbg_calc_parser_passes"))

      os.write.over(
        os.pwd / "dbg_calc_parser" / "test_output.dbg",
        top.toPrettyWritable(CalcReader.wellformed),
        createFolders = true
      )

      top

    def evaluate: Node.Top = 
      val top = parse

      CalcEvaluator(top, tracer = Manip.RewriteDebugTracer(os.pwd / "dbg_calc_evaluator_passes"))

      os.write.over(
        os.pwd / "dbg_calc_evaluator" / "test_output.dbg",
        top.toPrettyWritable(CalcReader.wellformed),
        createFolders = true
      )

      top

  test("empty string"):
    assertEquals("".read, Node.Top())

  test("only whitespace"):
    assertEquals("    \n\t".read, Node.Top())

  test("error: invalid character"):
    assertEquals("k".read, Node.Top(Error("invalid byte", SourceMarker("k"))))

  test("single number"):
    assertEquals(
      "5".read, 
      Node.Top(
        tokens.Number("5")
      )
    )

  test("read basic addition"):
    assertEquals(
      "5 + 11".read,
      Node.Top(
        tokens.Number("5"),
        tokens.AddOp(),
        tokens.Number("11")
      )
    )

  test("read basic multiplication"):
    assertEquals(
      "5 * 11".read,
      Node.Top(
        tokens.Number("5"),
        tokens.MulOp(),
        tokens.Number("11")
      )
    )

  test("read full calculation"):
    assertEquals(
      "5 + 11 * 4".read,
      Node.Top(
        tokens.Number("5"),
        tokens.AddOp(),
        tokens.Number("11"),
        tokens.MulOp(),
        tokens.Number("4")
      )
    )

  test("simple addition parse"):
    assertEquals(
      "5 + 11".parse, 
      Node.Top(
        tokens.Expression(
          tokens.Add(
            tokens.Number("5"),
            tokens.Number("11")
          ).at("5 + 11")
        ).at("5 + 11")
      )
    )

  test("simple multiplication parse"):
    assertEquals(
      "5 * 11".parse, 
      Node.Top(
        tokens.Expression(
          tokens.Mul(
            tokens.Number("5"),
            tokens.Number("11")
          ).at("5 * 11")
        ).at("5 * 11")
      )
    )

  test("full calculation parse"):
    assertEquals(
      "5 + 11 * 4".parse,
      Node.Top(
        tokens.Expression(
          tokens.Add(
            tokens.Number("5"),
            tokens.Expression(
              tokens.Mul(
                tokens.Number("11"),
                tokens.Number("4")
              ).at("11 * 4")
            ).at("11 * 4")
          ).at("5 + 11 * 4")
        ).at("5 + 11 * 4")
      )
    )

  test("full calculation 4 parse"):
    assertEquals(
      "5 * 4 + 4 / 2 - 6 * 2".parse,
      Node.Top(
        tokens.Expression(
          tokens.Sub(
            tokens.Expression(
              tokens.Add(
                tokens.Expression(
                  tokens.Mul(
                    tokens.Number("5"),
                    tokens.Number("4")
                  ).at("5 * 4")
                ).at("5 * 4"),
                tokens.Expression(
                  tokens.Div(
                    tokens.Number("4"),
                    tokens.Number("2")
                  ).at("4 / 2")
                ).at("4 / 2")
              ).at("5 * 4 + 4 / 2")
            ).at("5 * 4 + 4 / 2"),
            tokens.Expression(
              tokens.Mul(
                tokens.Number("6"),
                tokens.Number("2")
              ).at("6 * 2")
            ).at("6 * 2")
          ).at("5 * 4 + 4 / 2 - 6 * 2")
        ).at("5 * 4 + 4 / 2 - 6 * 2")
      )
    )

  test("addition calculation"):
    assertEquals(
      "5 + 11".evaluate,
      Node.Top(
        tokens.Number("16")
      ))

  test("multiplication calculation"):
    assertEquals(
      "5 * 11".evaluate,
      Node.Top(
        tokens.Number("55")
      ))

  test("full calculation"):
    assertEquals(
      "5 + 11 * 4".evaluate,
      Node.Top(
        tokens.Number("49")
      ))

  test("full calculation 2"):
    assertEquals(
      "5 * 4 + 4 / 2".evaluate,
      Node.Top(
        tokens.Number("22")
      ))

  test("full calculation 3"):
    assertEquals(
      "5 * 4 + 4 / 2 - 6".evaluate,
      Node.Top(
        tokens.Number("16")
      ))

  test("full calculation 4"):
    assertEquals(
      "5 * 4 + 4 / 2 - 6 * 2".evaluate,
      Node.Top(
        tokens.Number("10")
      ))

  // test("error: incorrect format"):
  //   intercept[IllegalArgumentException] (
  //     "10 - 10 7".evaluate
  //   )

  // test("error: divide by zero"):
  //   intercept[ArithmeticException] (
  //     "16 * 2 / 0".evaluate
  //   )