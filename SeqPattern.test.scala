package distcompiler

import cats.syntax.all.given
import dsl.*

class SeqPatternTests extends munit.FunSuite:
  import PatternTests.*

  extension [T](pat: on[T])
    def onChildren(using munit.Location)(name: String)(children: Node.Child*)(
        expected: T
    ): pat.type =
      val top = Node.Top(children)
      test(name):
        val result =
          atNode(top)(atFirstChild(pat.value))
            .perform()
        assertEquals(result, expected)
      pat

  on(
    tok(tok1)
      | SeqPattern.pure("no")
  )
    .onChildren("tok: match tok1")(tok1())(tok1())
    .onChildren("tok: no match tok1")(tok2())("no")
    .onChildren("tok: empty")()("no")

  on(
    field(tok(tok1))
      ~ field(tok(tok2))
      ~ eof
      | SeqPattern.pure("no")
  )
    .onChildren("fields: match tok1, tok2")(tok1(), tok2())((tok1(), tok2()))
    .onChildren("fields: too short")(tok1())("no")
    .onChildren("fields: too long")(tok1(), tok2(), tok1())("no")
    .onChildren("fields: switched")(tok2(), tok1())("no")
    .onChildren("fields: empty")()("no")

  on(
    field(repeated(tok(tok1)))
      ~ eof
      | SeqPattern.pure("no")
  )
    .onChildren("fields repeated: 0")()(Nil)
    .onChildren("fields repeated: 1")(tok1())(List(tok1()))
    .onChildren("fields repeated: 2")(tok1(), tok1())(List(tok1(), tok1()))
    .onChildren("fields repeated: 3")(tok1(), tok1(), tok1())(
      List(tok1(), tok1(), tok1())
    )
    .onChildren("fields repeated: odd one out")(tok1(), tok2(), tok1())("no")
    .onChildren("fields repeated: prefix but end assert")(
      tok1(),
      tok1(),
      tok2()
    )("no")

  on(
    skip(tok(tok1))
      ~ field(tok(tok2))
      ~ skip(tok(tok3))
      ~ eof
      | SeqPattern.pure("no")
  )
    .onChildren("fields skips: exact")(tok1(), tok2(), tok3())(tok2())
    .onChildren("fields skip: first missing")(tok2(), tok3())("no")
    .onChildren("fields skip: last missing")(tok1(), tok2())("no")
    .onChildren("fields skip: empty")()("no")

object PatternTests:
  object tok1 extends Token
  object tok2 extends Token
  object tok3 extends Token
