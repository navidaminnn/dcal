package distcompiler.sexpr

import distcompiler.*
import Builtin.{Error, SourceMarker}

class parseTests extends munit.FunSuite:
  import tokens.Atom
  extension (str: String)
    def parse: Node.Top =
      sexpr.parse.fromSourceRange(SourceRange.entire(Source.fromString(str)))

  test("foo string literal"):
    assertEquals("\"foo\"".parse, Node.Top(Atom("foo")))

  test("escape tab char"):
    assertEquals("\"\\t\"".parse, Node.Top(Atom("\t")))

  test("ignore crlf"):
    assertEquals("\"\\\r\n\"".parse, Node.Top(Atom("")))

  test("error: invalid string escape"):
    assertEquals(
      "\"\\^\"".parse,
      Node.Top(
        Error("invalid string escape", Builtin.SourceMarker()),
        Atom("?")
      )
    )
