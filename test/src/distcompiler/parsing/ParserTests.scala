package test.distcompiler.parsing

import cats.*
import cats.data.*
import cats.syntax.all.given

import distcompiler.util.EvalList

import distcompiler.parsing.{Parser, SourceLocation, ErrorOps}

class ParserTests extends munit.FunSuite {
  type Elem = Char
  type Input = distcompiler.parsing.TokenInput[Elem, Error]
  type Error = String

  given ErrorOps[Elem, Error] with {
    def expectedEOF(sourceLocation: SourceLocation, actualElem: Elem): Error = "expectedEOF"
    def unexpectedEOF(sourceLocation: SourceLocation): Error = "unexpectedEOF"
  }

  val dummySourceLocation = SourceLocation.fileStart("<dummy>")

  val ops = Parser.Ops[Elem, Input, Error]
  import ops.{*, given}

  def elem(ch: Char): P[Char] =
    anyElem.constrain {
      case `ch` => Right(ch)
      case otherwise => Left(s"expected $ch, got $otherwise")
    }

  def elems(chs: Char*): P[List[Char]] =
    chs
      .map(ch => elem(ch).map(Chain.one))
      .reduce((l, r) => (l ~ r).map { case l ~ r => l ++ r })
      .map(_.toList)

  def testP[T](using munit.Location)(p: P[T])(input: List[Char], expected: T): Unit = {
    val result = p.parse(distcompiler.parsing.TokenInput(
      EvalList.fromIterable(input.map(Right(_))),
      dummySourceLocation,
      _ => dummySourceLocation))

    assert(clue(result).isRight, input)
    assertEquals(result.toOption.get._2.tokens.toList, Nil, "remaining input")

    assertEquals(result.toOption.get._1, expected, ("parsed result", input))
  }

  def testFail[T](using munit.Location)(p: P[T])(input: List[Char]): Unit = {
    val result = p.parse(distcompiler.parsing.TokenInput(
      EvalList.fromIterable(input.map(Right(_))),
      dummySourceLocation,
      _ => dummySourceLocation))

    result match {
      case Left(errors) => // that's fine
      case Right((_, remainingInput)) =>
        assert({
          clue(result)
          clue(remainingInput.tokens.toList)
          remainingInput.tokens.nonEmpty
        }, input)
    }
  }

  test("eof") {
    testP(eof)(Nil, ())
  }

  test("basic seq") {
    testP(anyElem ~ anyElem)(List('a', 'b'), 'a' ~ 'b')
  }

  test("leftrec (prepend)") {
    lazy val pp: P[String] = lzy {
      (pp ~ anyElem).map { case l ~ r => l :+ r }
      | anyElem.map(_.toString())
    }

    testP(pp)(List('a'), "a")
    testP(pp)(List('a', 'b'), "ab")
    testP(pp)(List('a', 'b', 'c'), "abc")
    testP(pp)(List('a', 'b', 'c', 'd'), "abcd")
  }

  test("leftrec (bug: parsing an unexpected sequence)") {
    val pp: P[String] = localRec[String] { rec =>
      (rec ~ elem('x')).map { case a ~ b => a :+ b }
      | elem('x').map(_.toString())
      | elem('y').map(_.toString())
    }

    testP(pp)(
      List('x'),
      "x",
    )
    testP(pp)(
      List('x', 'x'),
      "xx",
    )
    testP(pp)(
      List('y', 'x'),
      "yx",
    )
    testP(pp)(
      List('x', 'x', 'x'),
      "xxx",
    )
    testP(pp)(
      List('y', 'x', 'x'),
      "yxx",
    )
    testP(rep(pp).map(_.toList))(
      List('x', 'x', 'y'),
      List("xx", "y"),
    )
  }

  test("leftrec (left-associative op)") {
    enum Tree {
      case Leaf(ch: Char)
      case Branch(left: Tree, right: Tree)
    }
    import Tree.*

    lazy val pp: P[Tree] = lzy {
      (pp ~ elem('+') ~ anyElem.map(Leaf(_))).map { case lhs ~ _ ~ rhs => Branch(lhs, rhs) }
      | anyElem.map(Leaf(_))
    }

    testP(pp)(List('a'), Leaf('a'))
    testP(pp)(List('a', '+', 'b'), Branch(Leaf('a'), Leaf('b')))
    testP(pp)(List('a', '+', 'b', '+', 'c'), Branch(Branch(Leaf('a'), Leaf('b')), Leaf('c')))
  }

  test("leftrec (nested with basic precedence)") {
    enum Tree {
      case Leaf(ch: Char)
      case Branch1(left: Tree, right: Tree)
      case Branch2(left: Tree, right: Tree)
    }
    import Tree.*

    val pp0: P[Tree] = anyElem.constrain {
      case ch if Character.isAlphabetic(ch) => Right(Leaf(ch))
      case otherwise => Left(s"$otherwise was not alphabetic")
    }

    lazy val pp1: P[Tree] = lzy {
      (pp1 ~ elem('*') ~ pp0).map { case lhs ~ _ ~ rhs => Branch1(lhs, rhs) }
      | pp0
    }

    lazy val pp2: P[Tree] = lzy {
      (pp2 ~ elem('+') ~ pp1).map { case lhs ~ _ ~ rhs => Branch2(lhs, rhs) }
      | pp1
    }

    testP(pp2)(List('a'), Leaf('a'))
    testP(pp2)(List('a', '*', 'b'), Branch1(Leaf('a'), Leaf('b')))
    testP(pp2)(List('a', '+', 'b'), Branch2(Leaf('a'), Leaf('b')))
    testP(pp2)(List('a', '+', 'b', '*', 'c'), Branch2(Leaf('a'), Branch1(Leaf('b'), Leaf('c'))))
    testP(pp2)(List('a', '*', 'b', '+', 'c'), Branch2(Branch1(Leaf('a'), Leaf('b')), Leaf('c')))
    testP(pp2)(List('a', '*', 'b', '*', 'c'), Branch1(Branch1(Leaf('a'), Leaf('b')), Leaf('c')))
    testP(pp2)(List('a', '+', 'b', '+', 'c'), Branch2(Branch2(Leaf('a'), Leaf('b')), Leaf('c')))
  }

  test("mustMakeProgress") {
    lazy val pp1: P[Unit] = lzy {
      anyElem.as(()) <~ pp1
      | anyElem.as(())
    }

    assert(anyElem.mustMakeProgress)
    assert(pp1.mustMakeProgress)

    lazy val pp2: P[Unit] = lzy(pp2)

    assert(!pp2.mustMakeProgress)

    val pp3: P[Unit] = localRec { rec =>
      anyElem.as(()) <~ rec
      | anyElem.as(())
    }

    assert(pp3.mustMakeProgress)

    val pp4: P[Unit] = localRec { rec =>
      anyElem.as(())
      | rec
      | anyElem.as(())
    }

    assert(!pp4.mustMakeProgress)
  }

  test("precedenceTree") {
    given Order[Set[Int]] = Order.fromLessThan(_.subsetOf(_))

    enum Tree {
      case Leaf(ch: Char)
      case Branch(ord: Set[Int], lhs: Tree, rhs: Tree)
    }
    import Tree.*

    val pp: P[Tree] =
      precedenceTree[Set[Int], Tree]
        .bottomLevel {
          anyElem.constrain {
            case ch if Character.isAlphabetic(ch) => Right(Leaf(ch))
            case ch => Left(s"expected alphabetic char, got $ch")
          }
        }
        .levelAssoc(Set(1)) { rec => lower =>
          (rec ~? elem('1') ~ lower)
            .map { case lhs ~ _ ~ rhs => Branch(Set(1), lhs, rhs) }
        }
        .levelAssoc(Set(2)) { rec => lower =>
          (lower ~? elem('2') ~ rec)
            .map { case lhs ~ _ ~ rhs => Branch(Set(2), lhs, rhs) }
        }
        .level(Set(1, 2, 3)) { lower =>
          (lower ~? elem('3') ~ lower)
            .map { case lhs ~ _ ~ rhs => Branch(Set(1, 2, 3), lhs, rhs) }
        }
        .levelAssoc(Set(1, 2, 4)) { rec => lower =>
          (lower ~? elem('4') ~ rec)
            .map { case lhs ~ _ ~ rhs => Branch(Set(1, 2, 4), lhs, rhs) }
        }
        .parser

    testP(pp)(List('x'), Leaf('x'))

    testP(pp)(List('x', '1', 'y'), Branch(Set(1), Leaf('x'), Leaf('y')))
    testP(pp)(List('x', '2', 'y'), Branch(Set(2), Leaf('x'), Leaf('y')))
    testP(pp)(List('x', '3', 'y'), Branch(Set(1, 2, 3), Leaf('x'), Leaf('y')))
    testP(pp)(List('x', '4', 'y'), Branch(Set(1, 2, 4), Leaf('x'), Leaf('y')))

    testP(pp)(List('x', '1', 'y', '1', 'z'), Branch(Set(1), Branch(Set(1), Leaf('x'), Leaf('y')), Leaf('z')))
    testP(pp)(List('x', '2', 'y', '2', 'z'), Branch(Set(2), Leaf('x'), Branch(Set(2), Leaf('y'), Leaf('z'))))
    testFail(pp)(List('x', '3', 'y', '3', 'z'))
    testP(pp)(List('x', '4', 'y', '4', 'z'), Branch(Set(1, 2, 4), Leaf('x'), Branch(Set(1, 2, 4), Leaf('y'), Leaf('z'))))
    testFail(pp)(List('x', '3', 'y', '4', 'z'))

    testP(pp)(
      List('x', '3', 'y', '1', 'z', '4', 'p'),
      Branch(Set(1), Branch(Set(1, 2, 3), Leaf('x'), Leaf('y')), Branch(Set(1, 2, 4), Leaf('z'), Leaf('p'))),
    )
    testP(pp)(
      List('x', '3', 'y', '2', 'z', '4', 'p'),
      Branch(Set(2), Branch(Set(1, 2, 3), Leaf('x'), Leaf('y')), Branch(Set(1, 2, 4), Leaf('z'), Leaf('p'))),
    )
    testFail(pp)(
      List('x', '3', 'y', '4', 'z', '1', 'p'),
    )
    testFail(pp)(
      List('x', '1', 'y', '4', 'z', '2', 'p'),
    )

    testP(pp)(
      List('x', '3', 'y', '1', 'z', '4', 'p', '4', 'q'),
      Branch(Set(1), Branch(Set(1, 2, 3), Leaf('x'), Leaf('y')), Branch(Set(1, 2, 4), Leaf('z'), Branch(Set(1, 2, 4), Leaf('p'), Leaf('q')))),
    )

    testP(pp)(
      List('w', '3', 'x', '2', 'y', '2', 'z', '4', 'p', '4', 'q'),
      Branch(Set(2), Branch(Set(1, 2, 3), Leaf('w'), Leaf('x')), Branch(Set(2), Leaf('y'), Branch(Set(1, 2, 4), Leaf('z'), Branch(Set(1, 2, 4), Leaf('p'), Leaf('q'))))),
    )
  }

  test("first match") {
    val pp: P[List[Int]] = rep(elems('x', 'x').peek.as(1) | elems('x', 'x', 'x').peek.as(2)).map(_.toList)

    testP(pp)(
      Nil,
      Nil,
    )
    testFail(pp)(
      List('x'),
    )
    testP(pp)(
      List('x', 'x'),
      List(1),
    )
    testFail(pp)(
      List('x', 'x', 'x'),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x'),
      List(1, 1),
    )
    testFail(pp)(
      List('x', 'x', 'x', 'x', 'x'),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x'),
      List(1, 1, 1),
    )
    testFail(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x', 'x'),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x', 'x', 'x'),
      List(1, 1, 1, 1),
    )
    testFail(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x'),
    )
  }

  test("longest match") {
    val pp: P[List[Int]] = rep(elems('x', 'x').peek.as(1) ||| elems('x', 'x', 'x').peek.as(2)).map(_.toList)

    testP(pp)(
      Nil,
      Nil,
    )
    testFail(pp)(
      List('x'),
    )
    testP(pp)(
      List('x', 'x'),
      List(1),
    )
    testP(pp)(
      List('x', 'x', 'x'),
      List(2),
    )
    testFail(pp)(
      List('x', 'x', 'x', 'x'),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x', 'x'),
      List(2, 1),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x'),
      List(2, 2),
    )
    testFail(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x', 'x'),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x', 'x', 'x'),
      List(2, 2, 1),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x', 'x', 'x', 'x'),
      List(2, 2, 2),
    )
  }

  test("longest match tie breaker") {
    val pp: P[Int] =
      elems('x', 'x').as(1) ||| elems('x', 'x').as(2)

    testP(pp)(
      List('x', 'x'),
      1,
    )
  }

  test("all matches (but inefficient)") {
    val pp: P[List[Int]] = localRec[List[Int]] { rec =>
      (elems('x', 'x') ~>? rec).map(1 :: _)
      ||| (elems('x', 'x', 'x') ~>? rec).map(2 :: _)
      ||| trivial(Nil)
    }

    testP(pp)(
      Nil,
      Nil,
    )
    testFail(pp)(
      List('x'),
    )
    testP(pp)(
      List('x', 'x'),
      List(1),
    )
    testP(pp)(
      List('x', 'x', 'x'),
      List(2),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x'),
      List(1, 1),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x', 'x'),
      List(1, 2),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x'),
      List(1, 1, 1),
    )
    testP(pp)(
      List('x', 'x', 'x', 'x', 'x', 'x', 'x'),
      List(1, 1, 2),
    )
  }
}
