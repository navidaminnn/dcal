package test.distcompiler.transform

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
      .rewriting
      .refine[Data2] { rec => {
        case Leaf => Branch(Leaf, Leaf)
        case other => rec(other)
      }}
      .apply

    assertEquals(fn(Leaf), Branch(Leaf, Leaf))
    assertEquals(
      fn(Branch(Leaf, Branch(Leaf, Leaf))),
      Branch(Branch(Leaf, Leaf), Branch(Branch(Leaf, Leaf), Branch(Leaf, Leaf))),
    )
  }

  test("add 1 to nested ints") {
    import Data1.*

    val fn = Transformable[Data1]
      .rewriting
      .replace[Int](_ + 1)
      .apply

    assertEquals(Leaf, fn(Leaf))

    assertEquals(
      fn(IntOpt(Some((1, IntOpt(Some((2, Leaf))))))),
      IntOpt(Some(2, IntOpt(Some((3, Leaf))))),
    )

    assertEquals(
      fn(Branch(List(Leaf, Branch(List(IntOpt(None), IntOpt(Some((7, Leaf))))), IntOpt(Some((-1, Leaf)))))),
      Branch(List(Leaf, Branch(List(IntOpt(None), IntOpt(Some((8, Leaf))))), IntOpt(Some((0, Leaf))))),
    )
  }
}
