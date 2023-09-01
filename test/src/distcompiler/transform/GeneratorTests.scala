package test.distcompiler.transform

import cats.*
import cats.syntax.all.given

import distcompiler.transform.*

class GeneratorTests extends munit.FunSuite {
  import Generator.*

  enum Tree derives Generatable {
    case Empty
    case Branch(value: Int, left: Tree, right: Tree)
  }

  extension [T](self: Generator[T]) def depthsUpTo(n: Int): List[Example[T]] =
    self
      .examplesIterator
      .takeWhile(_.maxDepth <= n)
      .flatMap(_.flatten)
      .toList

  test("empty") {
    val gen = empty
    val actualRounds = gen.depthsUpTo(4)
    assertEquals(actualRounds, Nil)
  }

  test("single") {
    val gen = pure(1)
    val actualRounds = gen.depthsUpTo(3)
    assertEquals(actualRounds, List(Example(value = 1, maxDepth = 0)))
  }

  test("costly disjunction A") {
    val gen = lzy(lzy(pure(1)) | lzy(pure(2)))
    val actualRounds = gen.depthsUpTo(4)
    assertEquals(actualRounds,List(
      Example(value = 1, maxDepth = 2),
      Example(value = 2, maxDepth = 2),
    ))
  }

  test("costly disjunction B") {
    val gen = lzy(pure(1)) | lzy(pure(2))
    val actualRounds = gen.depthsUpTo(2)
    assertEquals(actualRounds, List(
      Example(value = 1, maxDepth = 1),
      Example(value = 2, maxDepth = 1),
    ))
  }

  test("cross product") {
    val gen = (pure(1) | pure(2)).map2(pure(3) | pure(4))((_, _))
    val actualrounds = gen.depthsUpTo(3)
    assertEquals(actualrounds, List(
      Example(value = (1, 3), maxDepth = 0),
      Example(value = (2, 3), maxDepth = 0),
      Example(value = (1, 4), maxDepth = 0),
      Example(value = (2, 4), maxDepth = 0),
    ))
  }

  test("andThen") {
    val gen = (pure(1) | pure(2)).andThen(x => pure(x * 5) | pure(x * 7))
    val actualRounds = gen.depthsUpTo(3)
    assertEquals(actualRounds, List(
      Example(value = 5, maxDepth = 0),
      Example(value = 7, maxDepth = 0),
      Example(value = 10, maxDepth = 0),
      Example(value = 14, maxDepth = 0),
    ))
  }

  test("andThen lzy before") {
    val gen = lzy(pure(1) | pure(2)).andThen(x => pure(x * 5) | pure(x * 7))
    val actualRounds = gen.depthsUpTo(3)
    assertEquals(actualRounds, List(
      Example(value = 5, maxDepth = 1),
      Example(value = 7, maxDepth = 1),
      Example(value = 10, maxDepth = 1),
      Example(value = 14, maxDepth = 1),
    ))
  }

  test("andThen lzy after") {
    val gen = (pure(1) | pure(2)).andThen(x => lzy(pure(x * 5) | pure(x * 7)))
    val actualRounds = gen.depthsUpTo(3)
    assertEquals(actualRounds, List(
      Example(value = 5, maxDepth = 1),
      Example(value = 7, maxDepth = 1),
      Example(value = 10, maxDepth = 1),
      Example(value = 14, maxDepth = 1),
    ))
  }

  test("andThen lzy both ways") {
    val gen = lzy(pure(1) | pure(2)).andThen(x => lzy(pure(x * 5) | pure(x * 7)))
    val actualRounds = gen.depthsUpTo(3)
    assertEquals(actualRounds, List(
      Example(value = 5, maxDepth = 2),
      Example(value = 7, maxDepth = 2),
      Example(value = 10, maxDepth = 2),
      Example(value = 14, maxDepth = 2),
    ))
  }

  test("lazy cross product") {
    val gen = (lzy(pure(1)) | lzy(pure(2))).map2(lzy(pure(3)) | lzy(pure(4)))((_, _))
    val actualrounds = gen.depthsUpTo(3)
    assertEquals(actualrounds, List(
      Example(value = (1, 3), maxDepth = 1),
      Example(value = (2, 3), maxDepth = 1),
      Example(value = (1, 4), maxDepth = 1),
      Example(value = (2, 4), maxDepth = 1),
    ))
  }

  test("tree generator") {
    lazy val gen: Generator[Tree] =
      pure(Tree.Empty)
      | lzy(gen.map2(gen)(Tree.Branch(42, _, _)))

    val actualRounds = gen.depthsUpTo(2)

    import Tree.*
    assertEquals(actualRounds, List(
      Example(value = Empty, maxDepth = 0),
      Example(value = Branch(value = 42, left = Empty, right = Empty), maxDepth = 1),
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
          right = Empty,
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

  test("tree generator (anyOf)") {
    val gen = Generatable[Tree]
      .build
      .replace[Int](pure(42))
      .apply
    val actualRounds = gen.depthsUpTo(6)

    import Tree.*
    assertEquals(actualRounds, List(
      Example(value = Empty, maxDepth = 2),
      Example(value = Branch(value = 42, left = Empty, right = Empty), maxDepth = 4),
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
          right = Empty,
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

  test("involves") {
    enum Tree derives Transformable {
      case Leaf(list: List[Int])
      case Branch(left: Tree, right: Tree)
    }

    val tree1 = Tree.Branch(Tree.Leaf(Nil), Tree.Leaf(List(16)))
    val tree2 = Tree.Branch(Tree.Leaf(List(5, 6, 7)), tree1)
    val tree3 = Tree.Branch(Tree.Leaf(List(0, -1, 8)), tree1)

    def countListsWithNegs(tree: Tree): Count =
      Transformable[Tree]
        .combining[Count]
        .replace[List[Int]](lst => Count.fromBoolean(lst.exists(_ < 0)))
        .apply(tree)

    assertEquals(countListsWithNegs(tree1), Count.empty)
    assertEquals(countListsWithNegs(tree2), Count.empty)
    assertEquals(countListsWithNegs(tree3), Count.one)

    def countNegs(tree: Tree): Count =
      Transformable[Tree]
        .combining[Count]
        .replace[Int](i => Count.fromBoolean(i < 0))
        .apply(tree)

    assertEquals(countNegs(tree1), Count.empty)
    assertEquals(countNegs(tree2), Count.empty)
    assertEquals(countNegs(tree3), Count.one)
  }
}
