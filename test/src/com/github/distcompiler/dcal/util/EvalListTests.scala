package test.com.github.distcompiler.dcal.util

import cats.*
import cats.syntax.all.given

import com.github.distcompiler.dcal.util.EvalList

class EvalListTests extends munit.FunSuite {
  test("convert to iterableOnce and back") {
    (0 to 5).foreach { length =>
      val list = (0 until length).toList

      assertEquals(EvalList.fromIterableOnce(list).toList, list)
    }

    // check that a "single" list works
    assertEquals(EvalList.single(42).toList, List(42))
  }

  test("counting with continually") {
    (0 to 5).foreach { length =>
      var idx = 0
    
      val countingList = 
        EvalList.continually {
          val result = idx
          idx += 1
          result
        }
          .takeWhile(_ <= length)

      assertEquals(countingList.toList, (0 to length).toList)
    }
  }

  test("concatenation") {
    val counts = (0 to 5).toList
    Applicative[List].product(counts, counts).foreach {
      case (lengthL, lengthR) =>
        val leftList = (0 until lengthL).toList
        val rightList = (0 until lengthR).toList

        assertEquals(
          (EvalList.fromIterableOnce(leftList) ++ EvalList.fromIterableOnce(rightList)).toList,
          leftList ++ rightList,
        )
    }
  }

  test("iterate a mutable thing twice") {
    val iter = Iterator.range(1, 5)

    val evalList = EvalList.fromIterableOnce(iter)

    assertEquals(evalList.toList, (1 until 5).toList)
    assertEquals(evalList.toList, (1 until 5).toList)
  }

  test("zipWithIndex") {
    assertEquals(
      EvalList.continually("foo").zipWithIndex.takeWhile(_._2 <= 3).toList,
      List(("foo", 0), ("foo", 1), ("foo", 2), ("foo", 3)),
    )
  }
  
  test("flatMap") {
    val evalList = EvalList.fromIterableOnce(Iterator.range(1, 3))

    assertEquals(evalList.flatMap(_ => evalList).toList, List(1, 2, 1, 2))
  }

  test("filter") {
    val evalList = EvalList.fromIterableOnce(Iterator.range(1, 5))

    assertEquals(evalList.filter(_ % 2 == 0).toList, List(2, 4))
  }

  test("take") {
    val evalList = EvalList.fromIterableOnce(Iterator.range(1, 5))

    assertEquals(evalList.take(0).toList, List())
    assertEquals(evalList.take(1).toList, List(1))
    assertEquals(evalList.take(2).toList, List(1, 2))
    assertEquals(evalList.take(3).toList, List(1, 2, 3))
    assertEquals(evalList.take(4).toList, List(1, 2, 3, 4))
    assertEquals(evalList.take(5).toList, List(1, 2, 3, 4))
  }

  test("drop") {
    val evalList = EvalList.fromIterableOnce(Iterator.range(1, 5))

    assertEquals(evalList.drop(0).toList, List(1, 2, 3, 4))
    assertEquals(evalList.drop(1).toList, List(2, 3, 4))
    assertEquals(evalList.drop(2).toList, List(3, 4))
    assertEquals(evalList.drop(3).toList, List(4))
    assertEquals(evalList.drop(4).toList, List())
    assertEquals(evalList.drop(5).toList, List())
  }
}
