package distcompiler.sexpr

import distcompiler.*
import Builtin.{Error, SourceMarker}

class readerTests extends munit.FunSuite:
  import tokens.Atom
  extension (str: String)
    def read: Node.Top =
      reader(SourceRange.entire(Source.fromString(str)))

  test("empty string"):
    assertEquals("".read, Node.Top())
  test("one space"):
    assertEquals(" ".read, Node.Top())
  test("all misc whitespace"):
    assertEquals(" \n\n \t\r\n".read, Node.Top())
  test("one atom"):
    assertEquals("8:deadbeef".read, Node.Top(Atom("deadbeef")))
  test("two atoms"):
    assertEquals("3:foo 3:bar".read, Node.Top(Atom("foo"), Atom("bar")))
  test("atom with weird chars"):
    assertEquals("9:(foo bar)".read, Node.Top(Atom("(foo bar)")))

  test("empty atom"):
    assertEquals("0:".read, Node.Top(Atom("")))
  test("empty list"):
    assertEquals("()".read, Node.Top(tokens.List()))

  test("list of two atoms"):
    assertEquals(
      "(3:foo 3:bar)".read,
      Node.Top(tokens.List(Atom("foo"), Atom("bar")))
    )
  test("list of three lists"):
    assertEquals(
      "((3:foo) (3:bar) ())".read,
      Node.Top(
        tokens.List(
          tokens.List(Atom("foo")),
          tokens.List(Atom("bar")),
          tokens.List()
        )
      )
    )

  test("error: stray list close"):
    assertEquals(
      ")".read,
      Node.Top(Error("unexpected end of list", SourceMarker()))
    )

  test("error: stray list close, recovery"):
    assertEquals(
      ") 3:foo".read,
      Node.Top(
        Error("unexpected end of list", SourceMarker()),
        Atom("foo")
      )
    )

  test("error: missing list terminator"):
    assertEquals(
      "(4: foo".read,
      Node.Top(
        tokens.List(Atom(" foo"), Error("unexpected EOF", SourceMarker()))
      )
    )

  test("empty string literal"):
    assertEquals("\"\"".read, Node.Top(tokens.String("")))

  test("3 empty string literals with a list"):
    assertEquals(
      "\"\" (\"\" ) \"\"".read,
      Node.Top(
        tokens.String(""),
        tokens.List(tokens.String("")),
        tokens.String("")
      )
    )

  test("list of 3 string literals"):
    assertEquals(
      raw"""("foo" "bar" " ")""".read,
      Node.Top(
        tokens.List(
          tokens.String("foo"),
          tokens.String("bar"),
          tokens.String(" ")
        )
      )
    )

  test("string with escapes"):
    assertEquals(
      raw""" "\tfoo\\bar" """.read,
      Node.Top(tokens.String("\\tfoo\\\\bar"))
    )

  test("invalid string escape"):
    assertEquals(
      raw""" "\^" """.read,
      Node.Top(
        // handled in parser
        // Error("invalid string escape", SourceMarker()),
        tokens.String("\\^")
      )
    )

  test("misc token atoms"):
    assertEquals(
      "foo bar =ping= :this /42a".read,
      Node.Top(
        Atom("foo"),
        Atom("bar"),
        Atom("=ping="),
        Atom(":this"),
        Atom("/42a")
      )
    )
