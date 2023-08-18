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
    assertEquals(EvalList.singleNow(42).toList, List(42))
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
}
