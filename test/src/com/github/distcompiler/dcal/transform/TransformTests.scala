package test.com.github.distcompiler.dcal.transform

import com.github.distcompiler.dcal.transform.instances.all.given
import com.github.distcompiler.dcal.transform.Transform

class TransformTests extends munit.FunSuite {
  enum Data1 derives CanEqual {
    case IntOpt(intOpt: Option[(Int, Data1)])
    case Branch(branches: List[Data1])
    case Leaf
  }

  enum Data2 derives CanEqual {
    case Leaf
    case Branch(left: Data2, right: Data2)
  }

  test("deepen tree by one") {
    import Data2.*
    val fn = Transform.Refined.fromPartialFunction[Data2, Data2] {
      case Leaf => Branch(Leaf, Leaf)
    }

    assertEquals(fn(Leaf), Branch(Leaf, Leaf))
    assertEquals(
      fn(Branch(Leaf, Branch(Leaf, Leaf))),
      Branch(Branch(Leaf, Leaf), Branch(Branch(Leaf, Leaf), Branch(Leaf, Leaf))),
    )
  }

  test("add 1 to nested ints") {
    import Data1.*
    given inc: Transform[Int, Int] = Transform.fromFunction(_ + 1)

    val fn = summon[Transform[Data1, Data1]]

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
