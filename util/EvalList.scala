package distcompiler.util

import cats.*
import cats.data.Ior
import cats.syntax.all.given
import cats.StackSafeMonad

opaque type EvalList[+T] = Eval[EvalList.Cell[T]]

object EvalList {
  enum Cell[+T] derives CanEqual {
    case Nil
    case Single(value: T)
    case Cons(head: T, tail: EvalList[T])
  }
  import Cell.*

  def empty[T]: EvalList[T] = Eval.now(Cell.Nil)

  def single[T](value: T): EvalList[T] = Eval.now(Cell.Single(value))

  def apply[T](values: T*): EvalList[T] = EvalList.fromIterable(values)

  def defer[T](list: => EvalList[T]): EvalList[T] =
    Eval.defer(list)

  def continually[T](fn: => T): EvalList[T] =
    Eval.later(Cons(fn, continually(fn)))

  def fromIterableOnce[T](iter: IterableOnce[T]): EvalList[T] = {
    val it = iter.iterator
    def impl(): EvalList[T] =
      Eval.later {
        if (it.hasNext) {
          Cons(it.next, impl())
        } else {
          Nil
        }
      }

    impl()
  }

  def fromIterable[T](iter: Iterable[T]): EvalList[T] =
    fromIterableOnce(iter.iterator)

  given instance: (MonoidK[EvalList] & Foldable[EvalList] & Align[EvalList] &
    StackSafeMonad[EvalList]) =
    new MonoidK[EvalList]
      with Foldable[EvalList]
      with Align[EvalList]
      with StackSafeMonad[EvalList] {
      def empty[A]: EvalList[A] = EvalList.empty

      def functor: Functor[EvalList] = this

      def combineK[A](x: EvalList[A], y: EvalList[A]): EvalList[A] =
        x ++ y

      def align[A, B](fa: EvalList[A], fb: EvalList[B]): EvalList[Ior[A, B]] =
        fa.align(fb)

      def pure[A](x: A): EvalList[A] =
        EvalList.single(x)

      def flatMap[A, B](fa: EvalList[A])(f: A => EvalList[B]): EvalList[B] =
        fa.flatMap(f)

      override def toIterable[A](fa: EvalList[A]): Iterable[A] =
        new Iterable[A] {
          def iterator: Iterator[A] = fa.iterator
        }

      def foldLeft[A, B](fa: EvalList[A], b: B)(f: (B, A) => B): B =
        fa.iterator.foldLeft(b)(f)

      def foldRight[A, B](fa: EvalList[A], lb: Eval[B])(
          f: (A, Eval[B]) => Eval[B]
      ): Eval[B] = {
        def impl(fa: EvalList[A], lb: Eval[B]): Eval[B] =
          fa.flatMap {
            case Nil              => lb
            case Single(value)    => f(value, lb)
            case Cons(head, tail) => f(head, impl(tail, lb))
          }

        impl(fa, lb)
      }
    }

  extension [T](elem: => T) {
    def +:(self: EvalList[T]): EvalList[T] =
      Eval.later(Cons(elem, self))
  }

  extension [T](self: EvalList[T]) {
    def cell: Cell[T] = self.value

    def asEvalCell: Eval[Cell[T]] = self

    def isEmpty: Boolean =
      self.cell match {
        case Nil                    => true
        case Single(_) | Cons(_, _) => false
      }

    def nonEmpty: Boolean = !self.isEmpty

    def uncons: Option[(T, EvalList[T])] =
      self.cell match {
        case Nil              => None
        case Single(value)    => Some((value, EvalList.empty))
        case Cons(head, tail) => Some((head, tail))
      }

    def toList: List[T] =
      self.iterator.toList

    def iterator: Iterator[T] =
      new Iterator[T] {
        private var curr: Eval[Cell[T]] = self.memoize

        override def hasNext: Boolean =
          curr.value match {
            case Nil        => false
            case Single(_)  => true
            case Cons(_, _) => true
          }

        override def next(): T =
          curr.value match {
            case Nil => throw java.util.NoSuchElementException()
            case Single(value) =>
              curr = Eval.now(Nil)
              value
            case Cons(head, tail) =>
              curr =
                tail.memoize // don't "eval" the tail yet; delay as much as possible
              head
          }
      }

    def ++(other: EvalList[T]): EvalList[T] =
      self.flatMap {
        case Nil              => other
        case Single(value)    => Eval.now(Cons(value, other))
        case Cons(head, tail) => Eval.now(Cons(head, tail ++ other))
      }

    def tapEach[U](fn: T => U): EvalList[T] =
      EvalList.map(self) { value =>
        fn(value)
        value
      }

    def flatten[U](using T <:< EvalList[U]): EvalList[U] =
      self.flatMap(identity)

    def filter(pred: T => Boolean): EvalList[T] =
      self.flatMap {
        case Nil                          => Eval.now(Nil)
        case Single(value) if pred(value) => Eval.now(Single(value))
        case Single(_)                    => Eval.now(Nil)
        case Cons(head, tail) if pred(head) =>
          Eval.now(Cons(head, tail.filter(pred)))
        case Cons(_, tail) => tail.filter(pred)
      }

    def take(n: Int): EvalList[T] =
      self.slice(0, n `max` 0)

    def drop(n: Int): EvalList[T] =
      self.slice(n, -1)

    def slice(from: Int, to: Int): EvalList[T] = {
      val lo = from `max` 0
      val hi = if (lo < 0) -1 else to

      if (hi == 0) {
        Eval.now(Nil)
      } else {
        if (lo == 0) {
          if (hi == -1) {
            self
          } else {
            self.map {
              case Nil              => Nil
              case Single(value)    => Single(value)
              case Cons(head, tail) => Cons(head, tail.slice(0, hi - 1))
            }
          }
        } else {
          self.flatMap {
            case Nil | Single(_) => Eval.now(Nil)
            case Cons(_, tail)   => tail.slice(lo - 1, hi - 1)
          }
        }
      }
    }

    def memoize: EvalList[T] = {
      def impl(self: Eval[Cell[T]]): Eval[Cell[T]] =
        self.map {
          case Nil              => Nil
          case Single(value)    => Single(value)
          case Cons(head, tail) => Cons(head, impl(tail))
        }.memoize

      impl(self)
    }

    def takeWhile(pred: T => Boolean): EvalList[T] =
      self.map {
        case Nil                            => Nil
        case Single(value) if pred(value)   => Single(value)
        case Single(_)                      => Nil
        case Cons(head, tail) if pred(head) => Cons(head, tail.takeWhile(pred))
        case Cons(_, _)                     => Nil
      }

    def align[U](other: EvalList[U]): EvalList[Ior[T, U]] =
      self.alignWith(other)(identity)

    def alignWith[U, V](other: EvalList[U])(fn: Ior[T, U] => V): EvalList[V] =
      (Semigroupal[Eval]
        .product(self, other))
        .flatMap {
          case (Nil, Nil) => Eval.now(Nil)
          case (notNil, Nil) =>
            EvalList.map(Eval.now(notNil))(v => fn(Ior.Left(v)))
          case (Nil, notNil) =>
            EvalList.map(Eval.now(notNil))(v => fn(Ior.Right(v)))
          case (Single(valueL), Single(valueR)) =>
            Eval.now(Single(fn(Ior.Both(valueL, valueR))))
          case (Cons(headL, tailL), Single(valueR)) =>
            Eval.now(
              Cons(
                fn(Ior.Both(headL, valueR)),
                EvalList.map(tailL)(v => fn(Ior.Left(v)))
              )
            )
          case (Single(valueL), Cons(headR, tailR)) =>
            Eval.now(
              Cons(
                fn(Ior.Both(valueL, headR)),
                EvalList.map(tailR)(v => fn(Ior.Right(v)))
              )
            )
          case (Cons(headL, tailL), Cons(headR, tailR)) =>
            Eval.now(
              Cons(
                fn(Ior.Both(headL, headR)),
                EvalList.alignWith(tailL)(tailR)(fn)
              )
            )
        }

    def interleave[U >: T](other: EvalList[U]): EvalList[U] =
      (Semigroupal[Eval]
        .product(self, other))
        .flatMap {
          case (Nil, Nil)                       => Eval.now(Nil)
          case (notNil, Nil)                    => Eval.now(notNil)
          case (Nil, notNil)                    => Eval.now(notNil)
          case (Single(valueL), Single(valueR)) => EvalList(valueL, valueR)
          case (Cons(headL, tailL), Single(valueR)) =>
            EvalList(headL, valueR) ++ tailL
          case (Single(valueL), Cons(headR, tailR)) =>
            EvalList(valueL, headR) ++ tailR
          case (Cons(headL, tailL), Cons(headR, tailR)) =>
            EvalList(headL, headR) ++ EvalList.interleave(tailL)(tailR)
        }

    def map[U](fn: T => U): EvalList[U] =
      self.map {
        case Nil => Nil
        case Single(value) =>
          Single(fn(value))
        case Cons(head, tail) =>
          Cons(fn(head), EvalList.map(tail)(fn))
      }

    def flatMap[U](fn: T => EvalList[U]): EvalList[U] =
      self.flatMap {
        case Cell.Nil           => Eval.now(Cell.Nil)
        case Cell.Single(value) => fn(value)
        case Cell.Cons(head, tail) =>
          fn(head) ++ EvalList.flatMap(tail)(fn)
      }

    def collect[U](fn: PartialFunction[T, U]): EvalList[U] =
      EvalList.flatMap(self) { value =>
        fn.unapply(value) match {
          case None        => EvalList.empty
          case Some(value) => EvalList.single(value)
        }
      }

    def zipWithIndex: EvalList[(T, Int)] = {
      def impl(self: EvalList[T], level: Int): EvalList[(T, Int)] =
        self.map {
          case Nil => Nil
          case Single(value) =>
            Single((value, level))
          case Cons(head, tail) =>
            Cons((head, level), impl(tail, level = level + 1))
        }

      impl(self, level = 0)
    }
  }

  extension [T](evalCell: Eval[Cell[T]]) {
    def asEvalList: EvalList[T] = evalCell
  }
}
