package test.com.github.distcompiler.dcal.chungus

import cats.syntax.all.given
import utest.{TestSuite, Tests, test}

object GeneratorTests extends TestSuite {
  import Generator.*

  enum Tree {
    case Empty
    case Branch(value: Int, left: Tree, right: Tree)
  }

  extension [T](self: Generator[T]) def depthsUpTo(n: Int): List[Example[T]] =
    self
      .examplesIterator
      .takeWhile(_.maxDepth <= n)
      .toList

  def tests = Tests {
    test("empty") {
      val gen = empty
      val actualRounds = gen.depthsUpTo(4)
      recording(actualRounds) {
        assert(actualRounds == Nil)
      }
    }
    test("single") {
      val gen = pure(1)
      val actualRounds = gen.depthsUpTo(3)
      recording(actualRounds) {
        assert(actualRounds == List(Example(value = 1, maxDepth = 0)))
      }
    }
    test("single (forced)") {
      val gen = force_!(pure(1))
      val actualRounds = gen.depthsUpTo(3)
      recording(actualRounds) {
        assert(actualRounds == List(Example(value = 1, maxDepth = 0)))
      }
    }
    test("costly disjunction A") {
      val gen = lzy(lzy(pure(1)) | lzy(pure(2)))
      val actualRounds = gen.depthsUpTo(2)
      recording(actualRounds) {
        assert(actualRounds == List(
          Example(value = 1, maxDepth = 2),
          Example(value = 2, maxDepth = 2),
        ))
      }
    }
    test("costly disjunction B") {
      val gen = lzy(pure(1)) | lzy(pure(2))
      val actualRounds = gen.depthsUpTo(2)
      recording(actualRounds) {
        assert(actualRounds == List(
          Example(value = 1, maxDepth = 1),
          Example(value = 2, maxDepth = 1),
        ))
      }
    }
    test("cross product") {
      val gen = (pure(1) | pure(2)).map2(pure(3) | pure(4))((_, _))
      val actualrounds = gen.depthsUpTo(3)
      recording(actualrounds) {
        assert(actualrounds == List(
          Example(value = (1, 3), maxDepth = 0),
          Example(value = (2, 3), maxDepth = 0),
          Example(value = (1, 4), maxDepth = 0),
          Example(value = (2, 4), maxDepth = 0),
        ))
      }
    }
    test("lazy cross product") {
      val gen = (lzy(pure(1)) | lzy(pure(2))).map2(lzy(pure(3)) | lzy(pure(4)))((_, _))
      val actualrounds = gen.depthsUpTo(3)
      recording(actualrounds) {
        assert(actualrounds == List(
          Example(value = (1, 3), maxDepth = 1),
          Example(value = (2, 3), maxDepth = 1),
          Example(value = (1, 4), maxDepth = 1),
          Example(value = (2, 4), maxDepth = 1),
        ))
      }
    }
    test("tree generator") {
      lazy val gen: Generator[Tree] =
        pure(Tree.Empty)
        | lzy(gen.map2(gen)(Tree.Branch(42, _, _)))

      val actualRounds = gen.depthsUpTo(2)
      recording(actualRounds) {
        import Tree.*
        assert(actualRounds == List(
          Example(value = Empty, maxDepth = 0),
          Example(value = Branch(value = 42, left = Empty, right = Empty), maxDepth = 1),
          Example(
            value = Branch(
              value = 42,
              left = Branch(value = 42, left = Empty, right = Empty),
              right = Empty,
            ),
            maxDepth = 2,
          ),
          Example(
            value = Branch(
              value = 42,
              left = Empty,
              right = Branch(value = 42, left = Empty, right = Empty),
            ),
            maxDepth = 2,
          ),
          Example(
            value = Branch(
              value = 42,
              left = Branch(value = 42, left = Empty, right = Empty),
              right = Branch(value = 42, left = Empty, right = Empty),
            ),
            maxDepth = 2,
          ),
        ))
      }
    }
    test("tree generator (anyOf)") {
      given Generator[Int] = pure(42)
      val gen = anyOf[Tree]
      val actualRounds = gen.depthsUpTo(6)
      recording(actualRounds) {
        import Tree.*
        assert(actualRounds == List(
          Example(value = Empty, maxDepth = 2),
          Example(value = Branch(value = 42, left = Empty, right = Empty), maxDepth = 4),
          Example(
            value = Branch(
              value = 42,
              left = Branch(value = 42, left = Empty, right = Empty),
              right = Empty,
            ),
            maxDepth = 6,
          ),
          Example(
            value = Branch(
              value = 42,
              left = Empty,
              right = Branch(value = 42, left = Empty, right = Empty),
            ),
            maxDepth = 6,
          ),
          Example(
            value = Branch(
              value = 42,
              left = Branch(value = 42, left = Empty, right = Empty),
              right = Branch(value = 42, left = Empty, right = Empty),
            ),
            maxDepth = 6,
          ),
        ))
      }
    }
  }
}
