package distcompiler.sexpr

import utest.*
import distcompiler.*
import Builtin.{Error, SourceMarker}

object parseTests extends TestSuite:
  def tests = Tests:
    import tokens.Atom
    extension (str: String)
      def parse: Node.Top =
        sexpr.parse.fromSourceRange(SourceRange.entire(Source.fromString(str)))

    test("foo string literal"):
      "\"foo\"".parse ==> Node.Top(Atom("foo"))

    test("escape tab char"):
      "\"\\t\"".parse ==> Node.Top(Atom("\t"))

    test("ignore crlf"):
      "\"\\\r\n\"".parse ==> Node.Top(Atom(""))

    test("error: invalid string escape"):
      "\"\\^\"".parse ==> Node.Top(
        Error("invalid string escape", Builtin.SourceMarker()),
        Atom("?")
      )
