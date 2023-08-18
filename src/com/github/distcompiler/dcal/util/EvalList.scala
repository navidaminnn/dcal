package com.github.distcompiler.dcal.util

import cats.Eval
import cats.data.Ior
import cats.syntax.all.given

opaque type EvalList[+T] = Eval[EvalList.Cell[T]]

object EvalList {
  enum Cell[+T] derives CanEqual {
    case Nil
    case Single(value: Eval[T])
    case Cons(head: Eval[T], tail: EvalList[T])
  }
  import Cell.*

  def empty[T]: EvalList[T] = Eval.now(Cell.Nil)

  def singleNow[T](value: T): EvalList[T] = Eval.now(Cell.Single(Eval.now(value)))

  def singleLater[T](value: =>T): EvalList[T] = Eval.now(Cell.Single(Eval.later(value)))

  def singleAlways[T](value: =>T): EvalList[T] = Eval.now(Cell.Single(Eval.always(value)))

  def continually[T](fn: =>T): EvalList[T] =
    Eval.now(Cons(Eval.later(fn), Eval.defer(continually(fn)).memoize)) // this structure makes sure we don't re-eval fn

  def fromIterableOnce[T](iter: IterableOnce[T]): EvalList[T] = {
    val it = iter.iterator

    val optList = 
      EvalList.continually(it.nextOption())
        .takeWhile(_.nonEmpty)
    
    EvalList.map(optList)(_.get)
  }

  extension [T](elem: =>T) {
    def +:(self: =>EvalList[T]): EvalList[T] =
      Eval.now(Cons(Eval.later(elem), Eval.defer(self)))
  }

  extension [T](self: EvalList[T]) {
    def cell: Cell[T] = self.value

    def asEvalCell: Eval[Cell[T]] = self

    def memoize: EvalList[T] = self.memoize

    def isEmpty: Boolean =
      self.cell match {
        case Nil => true
        case Single(_) | Cons(_, _) => false
      }

    def nonEmpty: Boolean = !self.isEmpty

    def uncons: Option[(T, EvalList[T])] =
      self.cell match {
        case Nil => None
        case Single(value) => Some((value.value, EvalList.empty))
        case Cons(head, tail) => Some((head.value, tail))
      }

    def toList: List[T] =
      self.iterator.toList

    def iterator: Iterator[T] =
      new Iterator[T] {
        private var curr: Eval[Cell[T]] = self.memoize

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

  extension [T](self: =>EvalList[T]) {
    def ++(other: =>EvalList[T]): EvalList[T] = {
      def impl(self: EvalList[T]): EvalList[T] =
        self
          .flatMap {
            case Nil => Eval.defer(other)
            case Single(value) => Eval.now(Cons(value, Eval.defer(other)))
            case Cons(head, tail) => Eval.now(Cons(head, impl(tail)))
          }
          .memoize

      impl(Eval.defer(self))
    }

    def takeWhile(pred: T => Boolean): EvalList[T] = {
      def impl(self: EvalList[T]): EvalList[T] =
        self
          .asEvalCell
          .flatMap {
            case Nil => Eval.now(Nil)
            case Single(value) =>
              value.map { value =>
                if(pred(value)) {
                  Single(Eval.now(value))
                } else {
                  Nil
                }
              }
            case Cons(head, tail) =>
              head.flatMap { head =>
                if(pred(head)) {
                  Eval.now(Cons(Eval.now(head), impl(tail)))
                } else {
                  Eval.now(Nil)
                }
              }
          }
          .memoize

      impl(Eval.defer(self))
    }

    def zipIor[U](other: =>EvalList[U]): EvalList[Ior[T, U]] =
      (Eval.defer(self).asEvalCell.product(Eval.defer(other)))
        .flatMap {
          case (Nil, Nil) => Eval.now(Nil)
          case (notNil, Nil) => EvalList.map(Eval.now(notNil))(Ior.Left(_))
          case (Nil, notNil) => EvalList.map(Eval.now(notNil))(Ior.Right(_))
          case (Single(valueL), Single(valueR)) =>
            Eval.now(Single(valueL.map2(valueR)(Ior.Both(_, _))))
          case (Cons(headL, tailL), Single(valueR)) =>
            Eval.now(Cons(headL.map2(valueR)(Ior.Both(_, _)), EvalList.map(tailL)(Ior.Left(_))))
          case (Single(valueL), Cons(headR, tailR)) =>
            Eval.now(Cons(valueL.map2(headR)(Ior.Both(_, _)), EvalList.map(tailR)(Ior.Right(_))))
          case (Cons(headL, tailL), Cons(headR, tailR)) =>
            Eval.now(Cons(headL.map2(headR)(Ior.Both(_, _)), EvalList.zipIor(tailL)(tailR)))
        }
        .memoize

    def map[U](fn: T => U): EvalList[U] =
      Eval.defer(self)
        .map {
          case Nil => Nil
          case Single(value) => 
            Single(value.map(fn))
          case Cons(head, tail) =>
            Cons(head.map(fn), tail.map(fn))
        }
        .memoize

    def flatMap[U](fn: T => EvalList[U]): EvalList[U] = {
      def impl(self: EvalList[T]): EvalList[U] =
        self
          .asEvalCell
          .flatMap {
            case Cell.Nil => Eval.now(Cell.Nil)
            case Cell.Single(value) => value.flatMap(fn)
            case Cell.Cons(head, tail) =>
              head.flatMap(fn) ++ impl(tail)
          }
          .memoize

      impl(Eval.defer(self))
    }

    def collect[U](fn: PartialFunction[T, U]): EvalList[U] =
      EvalList.flatMap(self) { value =>
        fn.unapply(value) match {
          case None => EvalList.empty
          case Some(value) => EvalList.singleNow(value)
        }
      }

    def zipWithIndex: EvalList[(T, Int)] = {
      def impl(self: EvalList[T], level: Int): EvalList[(T, Int)] =
        self
          .map {
            case Nil => Nil
            case Single(value) => 
              Single(value.map((_, level)))
            case Cons(head, tail) =>
              Cons(head.map((_, level)), impl(tail, level = level + 1))
          }
          .memoize

      impl(Eval.defer(self), level = 0)
    }
  }

  extension [T](evalCell: Eval[Cell[T]]) {
    def asEvalList: EvalList[T] = evalCell
  }
}
