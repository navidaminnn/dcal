package test.com.github.distcompiler.dcal.chungus

import utest.{TestSuite, Tests, test}

object GeneratorTests extends TestSuite {
  import Generator.*

  extension [T](self: Generator[T]) def roundsUpTo(n: Int): List[List[Option[T]]] =
    (0 to n).iterator
      .map(self.computeResultsForRound)
      .map(_.toList)
      .toList

  def tests = Tests {
    test("empty") {
      val gen = none
      val actualRounds = gen.roundsUpTo(4)
      recording(actualRounds) {
        assert(actualRounds == List(Nil, Nil, Nil, Nil, Nil))
      }
    }
    test("single (with cost)") {
      val gen = one(1)
      val actualRounds = gen.roundsUpTo(3)
      recording(actualRounds) {
        assert(actualRounds == List(Nil, List(Some(1)), List(None), List(None)))
      }
    }
    test("single (free)") {
      val gen = free_!(1)
      val actualRounds = gen.roundsUpTo(3)
      recording(actualRounds) {
        assert(actualRounds == List(List(Some(1)), List(None), List(None), List(None)))
      }
    }
    test("single (forced)") {
      val gen = one(1).force
      val actualRounds = gen.roundsUpTo(3)
      recording(actualRounds) {
        assert(actualRounds == List(List(Some(1)), List(None), List(None), List(None)))
      }
    }
    test("costly disjunction A") {
      val gen = costOne(lzy(free_!(1)) | lzy(free_!(2)))
      val actualRounds = gen.roundsUpTo(2)
      recording(actualRounds) {
        assert(actualRounds == List(
          Nil,
          List(Some(1), Some(2)),
          List(None, None),
        ))
      }
    }
    test("costly disjunction B") {
      val gen = lzy(one(1)) | lzy(one(2))
      val actualRounds = gen.roundsUpTo(2)
      recording(actualRounds) {
        assert(actualRounds == List(
          Nil,
          List(Some(1), Some(2)),
          List(None, None),
        ))
      }
    }
    test("costly disjunction B w flatMap (just once)") {
      val gen = lzy(free_!(1)).flatMap(_ => lzy(lzy(one(2)) | lzy(one(3))))
      val actualRounds = gen.roundsUpTo(2)
      recording(actualRounds) {
        assert(actualRounds == List(
          Nil,
          List(Some(2), Some(3)),
          List(None, None),
        ))
      }
    }
    test("costly disjunction B w flatMap") {
      val gen = lzy(free_!(1)).flatMap(x => lzy(lzy(one(2)) | lzy(one(3))).map((x, _)))
      val actualRounds = gen.roundsUpTo(2)
      recording(actualRounds) {
        assert(actualRounds == List(
          Nil,
          List(Some((1, 2)), Some((1, 3))),
          List(None, None),
        ))
      }
    }
    test("cross product") {
      val gen = (one(1) | one(2)).flatMap(left => (one(3) | one(4)).map((left, _)))
      val actualrounds = gen.roundsUpTo(3)
      recording(actualrounds) {
        assert(actualrounds == List(
          Nil,
          Nil,
          List(Some((1, 3)), Some((1, 4)), Some((2, 3)), Some(2, 4)),
          List(None, None, None, None),
        ))
      }
    }
    test("lazy cross product") {
      // forces the impl to actually use the flatMap code
      val gen = (lzy(one(1)) | lzy(one(2))).flatMap(left => (lzy(one(3)) | lzy(one(4))).map((left, _)))
      val actualrounds = gen.roundsUpTo(3)
      recording(actualrounds) {
        assert(actualrounds == List(
          Nil,
          Nil,
          List(Some((1, 3)), Some((1, 4)), Some((2, 3)), Some(2, 4)),
          List(None, None, None, None),
        ))
      }
    }
  }
}
