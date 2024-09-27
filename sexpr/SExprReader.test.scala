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
  }
