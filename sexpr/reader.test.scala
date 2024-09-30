package distcompiler.sexpr

import utest.*
import distcompiler.*
import Builtin.{Error, SourceMarker}

object readerTests extends TestSuite:
  val tests = Tests:
    import tokens.Atom
    extension (str: String)
      def read: Node.Top =
        reader(SourceRange.entire(Source.fromString(str)))

    test("empty string"):
      "".read ==> Node.Top()
    test("one space"):
      " ".read ==> Node.Top()
    test("all misc whitespace"):
      " \n\n \t\r\n".read ==> Node.Top()
    test("one atom"):
      "8:deadbeef".read ==> Node.Top(Atom("deadbeef"))
    test("two atoms"):
      "3:foo 3:bar".read ==> Node.Top(Atom("foo"), Atom("bar"))
    test("atom with weird chars"):
      "9:(foo bar)".read ==> Node.Top(Atom("(foo bar)"))

    test("empty atom"):
      "0:".read ==> Node.Top(Atom(""))
    test("empty list"):
      "()".read ==> Node.Top(tokens.List())

    test("list of two atoms"):
      "(3:foo 3:bar)".read ==> Node.Top(tokens.List(Atom("foo"), Atom("bar")))
    test("list of three lists"):
      "((3:foo) (3:bar) ())".read ==> Node.Top(
        tokens.List(
          tokens.List(Atom("foo")),
          tokens.List(Atom("bar")),
          tokens.List()
        )
      )

    test("error: stray list close"):
      ")".read ==> Node.Top(Error("unexpected end of list", SourceMarker()))

    test("error: stray list close, recovery"):
      ") 3:foo".read ==> Node.Top(
        Error("unexpected end of list", SourceMarker()),
        Atom("foo")
      )

    test("error: missing list terminator"):
      "(4: foo".read ==> Node.Top(
        tokens.List(Atom(" foo"), Error("unexpected EOF", SourceMarker()))
      )

    test("empty string literal"):
      "\"\"".read ==> Node.Top(tokens.String(""))

    test("3 empty string literals with a list"):
      "\"\" (\"\" ) \"\"".read ==> Node.Top(
        tokens.String(""),
        tokens.List(tokens.String("")),
        tokens.String("")
      )

    test("list of 3 string literals"):
      raw"""("foo" "bar" " ")""".read ==> Node.Top(
        tokens.List(
          tokens.String("foo"),
          tokens.String("bar"),
          tokens.String(" ")
        )
      )

    test("string with escapes"):
      raw""" "\tfoo\\bar" """.read ==> Node.Top(tokens.String("\\tfoo\\\\bar"))

    test("invalid string escape"):
      raw""" "\^" """.read ==> Node.Top(
        // handled in parser
        // Error("invalid string escape", SourceMarker()),
        tokens.String("\\^")
      )

    test("misc token atoms"):
      "foo bar =ping= :this /42a".read ==> Node.Top(
        Atom("foo"),
        Atom("bar"),
        Atom("=ping="),
        Atom(":this"),
        Atom("/42a")
      )
