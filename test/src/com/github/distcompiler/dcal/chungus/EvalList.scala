package test.com.github.distcompiler.dcal.chungus

import cats.Eval
import cats.data.Ior
import cats.syntax.all.given

private enum EvalList[+T] derives CanEqual {
  case Nil
  case Single(value: Eval[T])
  case Cons(head: Eval[T], tail: Eval[EvalList[T]])

  def zipIor[U](other: EvalList[U]): EvalList[Ior[T, U]] =
    (this, other) match {
      case (Nil, Nil) => Nil
      case (notNil, Nil) => notNil.map(Ior.Left(_))
      case (Nil, notNil) => notNil.map(Ior.Right(_))
      case (Single(valueL), Single(valueR)) =>
        Single(valueL.map2(valueR)(Ior.Both(_, _)))
      case (Cons(headL, tailL), Single(valueR)) =>
        Cons(headL.map2(valueR)(Ior.Both(_, _)), tailL.map(_.map(Ior.Left(_))))
      case (Single(valueL), Cons(headR, tailR)) =>
        Cons(valueL.map2(headR)(Ior.Both(_, _)), tailR.map(_.map(Ior.Right(_))))
      case (Cons(headL, tailL), Cons(headR, tailR)) =>
        Cons(headL.map2(headR)(Ior.Both(_, _)), tailL.map2(tailR)(_ `zipIor` _))
    }

  def zipWithIndex: EvalList[(T, Int)] = {
    def impl(self: EvalList[T], level: Int): EvalList[(T, Int)] =
      self match {
        case Nil => Nil
        case Single(value) => 
          Single(value.map((_, level)))
        case Cons(head, tail) =>
          Cons(head.map((_, level)), tail.map(impl(_, level = level + 1)))
      }

    impl(this, level = 0)
  }

  def take(count: Int): EvalList[T] = ???

  def map[U](fn: T => U): EvalList[U] =
    this match {
      case Nil => Nil
      case Single(value) => 
        Single(value.map(fn))
      case Cons(head, tail) =>
        Cons(head.map(fn), tail.map(_.map(fn)))
    }

  def iterator: Iterator[T] = {
    val self = this
    new Iterator[T] {
      private var curr: Eval[EvalList[T]] = Eval.now(self)

      override def hasNext: Boolean =
        curr.value match {
          case Nil => false
          case Single(_) => true
          case Cons(_, _) => true
        }
      
      override def next(): T =
        curr.value match {
          case Nil => throw java.util.NoSuchElementException()
          case Single(value) => 
            curr = Eval.now(Nil)
            value.value
          case Cons(head, tail) =>
            curr = tail.memoize // don't "eval" the tail yet; delay as much as possible
            head.value
        }
    }
  }
}

private object EvalList {
  import EvalList.*

  def empty[T]: EvalList[T] = Nil
}
