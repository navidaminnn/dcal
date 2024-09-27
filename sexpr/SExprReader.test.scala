package distcompiler.sexpr

import utest.*
import distcompiler.{SourceRange, Node, Source}
import distcompiler.Builtin.{Error, SourceMarker}

object SExprReaderTests extends TestSuite:
  val tests = Tests {
    import tokens.Atom
    extension (str: String)
      def parse: Node.Top =
        SExprReader(SourceRange.entire(Source.fromString(str)))

    test("empty string"):
      "".parse ==> Node.Top()
    test("one space"):
      " ".parse ==> Node.Top()
    test("all misc whitespace"):
      " \n\n \t\r\n".parse ==> Node.Top()
    test("one atom"):
      "8:deadbeef".parse ==> Node.Top(Atom("deadbeef"))
    test("two atoms"):
      "3:foo 3:bar".parse ==> Node.Top(Atom("foo"), Atom("bar"))
    test("atom with weird chars"):
      "9:(foo bar)".parse ==> Node.Top(Atom("(foo bar)"))

    test("empty atom"):
      "0:".parse ==> Node.Top(Atom(""))
    test("empty list"):
      "()".parse ==> Node.Top(tokens.List())

    test("list of two atoms"):
      "(3:foo 3:bar)".parse ==> Node.Top(tokens.List(Atom("foo"), Atom("bar")))
    test("list of three lists"):
      "((3:foo) (3:bar) ())".parse ==> Node.Top(
        tokens.List(
          tokens.List(Atom("foo")),
          tokens.List(Atom("bar")),
          tokens.List()
        )
      )

    test("error: stray list close"):
      ")".parse ==> Node.Top(Error("unexpected end of list", SourceMarker()))

    test("error: stray list close, recovery"):
      ") 3:foo".parse ==> Node.Top(
        Error("unexpected end of list", SourceMarker()),
        Atom("foo")
      )

    test("error: missing list terminator"):
      "(4: foo".parse ==> Node.Top(
        tokens.List(Atom(" foo"), Error("unexpected EOF", SourceMarker()))
      )

    test("empty string literal"):
      "\"\"".parse ==> Node.Top(tokens.String(""))

    test("3 empty string literals with a list"):
      "\"\" (\"\" ) \"\"".parse ==> Node.Top(
        tokens.String(""),
        tokens.List(tokens.String("")),
        tokens.String("")
      )

    test("list of 3 string literals"):
      raw"""("foo" "bar" " ")""".parse ==> Node.Top(
        tokens.List(
          tokens.String("foo"),
          tokens.String("bar"),
          tokens.String(" ")
        )
      )

    test("string with escapes"):
      raw""" "\tfoo\\bar" """.parse ==> Node.Top(tokens.String("\\tfoo\\\\bar"))

    test("error: invalid string escape"):
      raw""" "\^" """.parse ==> Node.Top(
        Error("invalid string escape", SourceMarker()),
        tokens.String("\\^")
      )

    test("misc token atoms"):
      "foo bar =ping= :this /42a".parse ==> Node.Top(
        Atom("foo"),
        Atom("bar"),
        Atom("=ping="),
        Atom(":this"),
        Atom("/42a")
      )
  }
