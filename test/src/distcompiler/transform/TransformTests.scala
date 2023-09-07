package test.distcompiler.transform

import cats.*

import distcompiler.transform.*

class TransformTests extends munit.FunSuite {
  enum Data1 derives CanEqual, Transformable {
    case IntOpt(intOpt: Option[(Int, Data1)])
    case Branch(branches: List[Data1])
    case Leaf
  }

  enum Data2 derives CanEqual, Transformable {
    case Leaf
    case Branch(left: Data2, right: Data2)
  }

  test("deepen tree by one") {
    import Data2.*
    val fn = Transformable[Data2]
      .rewriting[Id, Eval]
      .refine[Data2] { rec => {
        case Leaf => Eval.now(Branch(Leaf, Leaf))
        case other => rec(other)
      }}
      .make

    assertEquals(fn(Leaf).value, Branch(Leaf, Leaf))
    assertEquals(
      fn(Branch(Leaf, Branch(Leaf, Leaf))).value,
      Branch(Branch(Leaf, Leaf), Branch(Branch(Leaf, Leaf), Branch(Leaf, Leaf))),
    )
  }

  test("add 1 to nested ints") {
    import Data1.*

    val fn = Transformable[Data1]
      .rewriting[Id, Eval]
      .replace[Int](i => Eval.now(i + 1))
      .make

    assertEquals(fn(Leaf).value, Leaf)

    assertEquals(
      fn(IntOpt(Some((1, IntOpt(Some((2, Leaf))))))).value,
      IntOpt(Some(2, IntOpt(Some((3, Leaf))))),
    )

    assertEquals(
      fn(Branch(List(Leaf, Branch(List(IntOpt(None), IntOpt(Some((7, Leaf))))), IntOpt(Some((-1, Leaf)))))).value,
      Branch(List(Leaf, Branch(List(IntOpt(None), IntOpt(Some((8, Leaf))))), IntOpt(Some((0, Leaf))))),
    )
  }

  test("count subtrees") {
    import Data1.*

    val fn = Transformable[Data1]
      .combining[Id, Count]
      .incrementAt[Data1](_ => true)
      .make

    assertEquals(fn(Leaf), Count(count = 1, depth = 1))

    assertEquals(
      fn(IntOpt(Some((1, IntOpt(Some((2, Leaf))))))),
      Count(count = 3, depth = 3),
    )

    assertEquals(
      fn(Branch(List(Leaf, Branch(List(IntOpt(None), IntOpt(Some((7, Leaf))))), IntOpt(Some((-1, Leaf)))))),
      Count(count = 8, depth = 4),
    )
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
        .combining[Id, Count]
        .replace[List[Int]](lst => Count.fromBoolean(lst.exists(_ < 0)))
        .make
        .apply(tree)

    assertEquals(countListsWithNegs(tree1), Count.empty)
    assertEquals(countListsWithNegs(tree2), Count.empty)
    assertEquals(countListsWithNegs(tree3), Count.one)

    def countNegs(tree: Tree): Count =
      Transformable[Tree]
        .combining[Id, Count]
        .replace[Int](i => Count.fromBoolean(i < 0))
        .make
        .apply(tree)

    assertEquals(countNegs(tree1), Count.empty)
    assertEquals(countNegs(tree2), Count.empty)
    assertEquals(countNegs(tree3), Count.one)
  }
}
