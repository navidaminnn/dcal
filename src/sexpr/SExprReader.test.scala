// Copyright 2024-2025 Forja Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package forja.sexpr

import forja.*
import forja.Builtin.{Error, SourceMarker}
import forja.sexpr.lang.{SAtom, SList}
import forja.source.{Source, SourceRange}

class SExprReaderTests extends munit.FunSuite:
  extension (str: String)
    def parse: Node.Top =
      sexpr.parse.fromSourceRange(SourceRange.entire(Source.fromString(str)))

  test("foo string literal"):
    assertEquals("\"foo\"".parse, Node.Top(SAtom("foo")))

  test("escape tab char"):
    assertEquals("\"\\t\"".parse, Node.Top(SAtom("\t")))

  test("ignore crlf"):
    assertEquals("\"\\\r\n\"".parse, Node.Top(SAtom("")))

  test("error: invalid string escape"):
    assertEquals(
      "\"\\^\"".parse,
      Node.Top(
        Error("invalid escape sequence \\^", Builtin.SourceMarker("\\^")),
        SAtom("?"),
      ),
    )

  test("empty string"):
    assertEquals("".parse, Node.Top())
  test("one space"):
    assertEquals(" ".parse, Node.Top())
  test("all misc whitespace"):
    assertEquals(" \n\n \t\r\n".parse, Node.Top())
  test("one atom"):
    assertEquals("8:deadbeef".parse, Node.Top(SAtom("deadbeef")))
  test("two atoms"):
    assertEquals("3:foo 3:bar".parse, Node.Top(SAtom("foo"), SAtom("bar")))
  test("atom with weird chars"):
    assertEquals("9:(foo bar)".parse, Node.Top(SAtom("(foo bar)")))

  test("empty atom"):
    assertEquals("0:".parse, Node.Top(SAtom("")))
  test("empty list"):
    assertEquals("()".parse, Node.Top(SList()))

  test("list of two atoms"):
    assertEquals(
      "(3:foo 3:bar)".parse,
      Node.Top(SList(SAtom("foo"), SAtom("bar"))),
    )
  test("list of three lists"):
    assertEquals(
      "((3:foo) (3:bar) ())".parse,
      Node.Top(
        SList(
          SList(SAtom("foo")),
          SList(SAtom("bar")),
          SList(),
        ),
      ),
    )

  test("error: stray list close"):
    assertEquals(
      ")".parse,
      Node.Top(Error("unexpected end of list", SourceMarker(")"))),
    )

  test("error: stray list close, recovery"):
    assertEquals(
      ") 3:foo".parse,
      Node.Top(
        Error("unexpected end of list", SourceMarker(")")),
        SAtom("foo"),
      ),
    )

  test("error: missing list terminator"):
    assertEquals(
      "(4: foo".parse,
      Node.Top(
        SList(SAtom(" foo"), Error("unexpected EOF", SourceMarker())),
      ),
    )

  test("empty string literal"):
    assertEquals("\"\"".parse, Node.Top(SAtom("")))

  test("3 empty string literals with a list"):
    assertEquals(
      "\"\" (\"\" ) \"\"".parse,
      Node.Top(
        SAtom(""),
        SList(SAtom("")),
        SAtom(""),
      ),
    )

  test("list of 3 string literals"):
    assertEquals(
      raw"""("foo" "bar" " ")""".parse,
      Node.Top(
        SList(
          SAtom("foo"),
          SAtom("bar"),
          SAtom(" "),
        ),
      ),
    )

  test("string with escapes"):
    assertEquals(
      raw""" "\tfoo\\bar" """.parse,
      Node.Top(SAtom("\tfoo\\bar")),
    )

  test("misc token atoms"):
    assertEquals(
      "foo bar =ping= :this /42a".parse,
      Node.Top(
        SAtom("foo"),
        SAtom("bar"),
        SAtom("=ping="),
        SAtom(":this"),
        SAtom("/42a"),
      ),
    )
