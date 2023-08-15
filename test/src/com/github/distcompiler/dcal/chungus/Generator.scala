package test.com.github.distcompiler.dcal.chungus

import com.github.distcompiler.dcal.transform.SummonTuple

import scala.annotation.targetName
import scala.util.Try
import scala.collection.mutable

import cats.data.{Chain, Ior}
import cats.*
import cats.implicits.given

enum Generator[T] {
  import Generator.*

  case Empty[T]() extends Generator[T]
  case Singleton(value: T)
  case Selection(values: Chain[T])
  case Log(gen: Generator[T], label: String)
  case Pay(genFn: () => Generator[T])
  case Filter(self: Generator[T], pred: T => Boolean)
  case Disjunction(left: Generator[T], right: Generator[T])
  case Ap[T, U](transform: Generator[T => U], self: Generator[T]) extends Generator[U]
  case AndThen[T, U](self: Generator[T], fn: T => Generator[U]) extends Generator[U]

  def filter(pred: T => Boolean): Generator[T] =
    Filter(this, pred)

  def log(label: String): Generator[T] =
    Log(this, label)

  def filterF(pred: Generator[T => Boolean]): Generator[T] =
    this.map2(pred) { (value, pred) =>
      if(pred(value)) {
        Some(value)
      } else {
        None
      }
    }
      .filter(_.nonEmpty)
      .map(_.get)

  def filterDistinct[U, F[_]](using T <:< F[U])(using Foldable[F]): Generator[T] =
    this.filterDistinctBy(identity)

  def filterDistinctBy[U, V, F[_]](using ev: T <:< F[U])(using Foldable[F])(fn: U => V): Generator[T] =
    this.filter { collection =>
      ev(collection).foldMap(u => Set(fn(u))).size == ev(collection).size
    }

  def force_! : Generator[T] =
    anyFromSeq {
      this.examplesIterator
        .flatMap(_.flatten)
        .map(_.value)
        .toSeq
    }

  def andThen[U](fn: T => Generator[U]): Generator[U] =
    AndThen(this, fn)

  def ++(using semigroup: Semigroup[T])(other: Generator[T]): Generator[T] =
    this.map2(other)(_ `combine` _)
  
  @targetName("or")
  def |[U](other: Generator[U]): Generator[T | U] =
    this.widen.combineK(other.widen)

  def toChecker: Checker.RootChecker[T] =
    Checker.fromGenerator(this)

  def examplesIterator: Iterator[Example[Option[T]]] = {
    extension [A](self: LazyList[A]) def zipIorLazyLists[B](other: LazyList[B]): LazyList[Ior[A, B]] =
      (self, other) match {
        case (selfHead #:: selfTail, otherHead #:: otherTail) =>
          Ior.Both(selfHead, otherHead) #:: (selfTail `zipIorLazyLists` otherTail)
        case (LazyList(), other) =>
          other.map(Ior.Right(_))
        case (self, LazyList()) =>
          self.map(Ior.Left(_))
      }

    extension [A](self: Iterator[A]) def zipIorIterators[B](other: Iterator[B]): Iterator[Ior[A, B]] =
      new Iterator[Ior[A, B]] {
        override def hasNext: Boolean = self.hasNext || other.hasNext
        override def next(): Ior[A, B] =
          if(self.hasNext && other.hasNext) {
            Ior.Both(self.next(), other.next())
          } else if(self.hasNext) {
            Ior.Left(self.next())
          } else {
            Ior.Right(other.next())
          }
      }

    def impl[T](self: Generator[T]): EvalList[Eval[Iterator[T]]] =
      self match {
        case Empty() =>
          EvalList.empty
        case Singleton(value) =>
          EvalList.Cons(Eval.now(Eval.always(Iterator.single(value))), Eval.now(EvalList.empty))
        case Selection(values) =>
          EvalList.Cons(Eval.now(Eval.always(values.iterator)), Eval.now(EvalList.empty))
        case Log(gen, label) =>
          impl(gen)
            .zipWithIndex
            .map {
              case (iterFn, depth) =>
                iterFn.map { iter =>
                  println(s"! instantiate $label @ depth $depth")
                  iter.tapEach { value =>
                    print(s"  $label @ depth $depth found ")
                    pprint.pprintln(value)
                  }
                }
            }
        case Pay(genFn) =>
          EvalList.Cons(Eval.now(Eval.always(Iterator.empty)), Eval.later(impl(genFn())))
        case Filter(self, pred) =>
          impl(self)
            .map(iterFn => iterFn.map(_.filter(pred)))
        case Disjunction(left, right) =>
          (impl(left) `zipIor` impl(right)).map {
            case Ior.Left(leftIterFn) => leftIterFn
            case Ior.Right(rightIterFn) => rightIterFn
            case Ior.Both(leftIterFn, rightIterFn) =>
              leftIterFn.map2(rightIterFn)(_ ++ _)
          }
        case ap: Ap[tt, T] =>
          val selfIterFns = impl(ap.self)
          val transIterFns = impl(ap.transform)

          def incSelf(selfIter: Iterator[tt], depth: Int): Iterator[Iterator[T]] =
            selfIter.flatMap { value =>
              transIterFns
                .iterator
                .take(depth + 1)
                .map { transIterFn =>
                  transIterFn.value.map(_.apply(value))
                }
            }

          def incTrans(transIter: Iterator[tt => T], depth: Int): Iterator[Iterator[T]] =
            transIter.flatMap { transform =>
              selfIterFns
                .iterator
                .take(depth + 1)
                .map { selfIterFn =>
                  selfIterFn.value.map(transform)
                }
            }

          (selfIterFns `zipIor` transIterFns)
            .zipWithIndex
            .map {
              case (Ior.Left(selfIterFn), depth) =>
                Eval.always(incSelf(selfIterFn.value, depth).flatten)
              case (Ior.Right(transIterFn), depth) =>
                Eval.always(incTrans(transIterFn.value, depth).flatten)
              case (Ior.Both(selfIterFn, transIterFn), depth) =>
                Eval.always {
                  (incSelf(selfIterFn.value, depth) `zipIorIterators` incTrans(transIterFn.value, depth - 1))
                    .flatMap {
                      case Ior.Both(selfIter, otherIter) => selfIter ++ otherIter
                      case Ior.Left(selfIter) => selfIter
                      case Ior.Right(otherIter) => otherIter
                    }
                }
            }
        case andThen: AndThen[tt, T] =>
          val self = andThen.self
          val fn = andThen.fn

          def weave(selfElems: EvalList[Eval[Iterator[tt]]], existingDepthList: EvalList[Eval[Iterator[T]]]): EvalList[Eval[Iterator[T]]] =
            selfElems match {
              case EvalList.Nil => existingDepthList
              case selfElems @ (EvalList.Single(_) | EvalList.Cons(_, _)) =>
                val (iterFn, deeperIterFns) =
                  selfElems match {
                    case EvalList.Single(value) => (value, Eval.now(EvalList.Nil))
                    case EvalList.Cons(head, tail) => (head, tail)
                  }

                var nextIterBufferUsed = false
                var reachedIdx = -1
                val nextIterBuffer = mutable.ListBuffer.empty[Eval[EvalList[Eval[Iterator[T]]]]]

                val (currentExistingIter, nextExistingDepthList) =
                  existingDepthList match {
                    case EvalList.Nil => (Eval.always(Iterator.empty), Eval.now(EvalList.Nil))
                    case EvalList.Single(value) => (value.flatten, Eval.now(EvalList.Nil))
                    case EvalList.Cons(head, tail) => (head.flatten, tail)
                  }

                val currentAdditionalIter =
                  iterFn
                    .flatMap { iterFn =>
                      iterFn.map { iter =>
                        iter
                          .zipWithIndex
                          .flatMap {
                            case (value, idx) =>
                              val result = impl(fn(value)) match {
                                case EvalList.Nil => Iterator.empty
                                case EvalList.Single(value) =>
                                  value.flatten.value
                                case EvalList.Cons(iter, nextIters) =>
                                  // note: do not under any circumstance evaluate nextIters!
                                  // doing that will make the assert below fail, because it breaks
                                  // the invariant: we will always exhaust shallower iters before
                                  // asking for deeper ones
                                  if(reachedIdx < idx) {
                                    assert(!nextIterBufferUsed)
                                    nextIterBuffer += nextIters
                                  }
                                  iter.flatten.value
                              }
                              
                              reachedIdx = idx `max` reachedIdx
                              result
                          }
                      }
                    }

                EvalList.Cons(
                  Eval.now(currentExistingIter.map2(currentAdditionalIter)(_ ++ _)),
                  for {
                    _ <- Eval.later { nextIterBufferUsed = true }
                    tail <- nextIterBuffer
                      .foldLeft(nextExistingDepthList)({ (acc, list) =>
                        acc.map2(list) { (acc: EvalList[Eval[Iterator[T]]], list: EvalList[Eval[Iterator[T]]]) =>
                          (acc `zipIor` list).map {
                            case Ior.Both(left, right) => left.map2(right)(_ ++ _)
                            case Ior.Left(iter) => iter
                            case Ior.Right(iter) => iter
                          }
                        }
                      })
                      .map2(deeperIterFns) { (nextExistingDepthList, nextSelfElems) => 
                        weave(nextSelfElems, nextExistingDepthList)
                      }
                      .memoize
                  } yield tail,
                )
            }

          weave(impl(self), EvalList.empty)
      }
      
    impl(this)
      .iterator
      .zipWithIndex
      .flatMap {
        case (iterFn, depth) =>
          iterFn.value.map { value =>
            Example(Some(value), maxDepth = depth)
          }
          ++ Iterator.single(Example(None, maxDepth = depth))
      }
  }
}

object Generator {
  import Generator.*

  final case class Example[T](value: T, maxDepth: Int) {
    def flatten[U](using ev: T <:< Option[U]): Option[Example[U]] =
      ev(value).map(Example(_, maxDepth))
  }

  given alternative: Alternative[Generator] with {
    override def empty[A]: Generator[A] = Empty()

    override def pure[A](x: A): Generator[A] = Singleton(x)

    override def ap[A, B](ff: Generator[A => B])(fa: Generator[A]): Generator[B] =
      Ap(ff, fa)

    override def combineK[A](x: Generator[A], y: Generator[A]): Generator[A] =
      (x, y) match {
        case (Empty(), right) => right
        case (left, Empty()) => left
        case (Singleton(leftV), Singleton(rightV)) => Selection(Chain(leftV, rightV))
        case (Singleton(leftV), Selection(rightVs)) => Selection(leftV +: rightVs)
        case (Selection(leftVs), Singleton(rightV)) => Selection(leftVs :+ rightV)
        case (Selection(leftVs), Selection(rightVs)) => Selection(leftVs ++ rightVs)
        case (left, right) => Disjunction(left, right)
      }
  }

  given monoid[T]: Monoid[Generator[T]] = alternative.algebra

  def empty[T]: Generator[T] = Empty()

  def pure[T](value: T): Generator[T] = value.pure

  def lzy[T](gen: =>Generator[T]): Generator[T] = {
    lazy val lzyGen = gen
    Pay(() => lzyGen)
  }

  extension [T](inline gen: Generator[T]) inline def dumpCode_! : Generator[T] =
    ${ DumpCode.impl('gen) }

  type TupleExcluding[T, Tpl <: Tuple] <: Tuple = Tpl match {
    case (T *: tl) => tl
    case (hd *: tl) => hd *: TupleExcluding[T, tl]
  }

  final class AnySumOfShape[Top, Tpl <: Tuple] {
    def excluding[TT]: AnySumOfShape[Top, TupleExcluding[Generator[TT], Tpl]] = new AnySumOfShape

    def summon(using partGens: =>SummonTuple.ST[Tpl]): Generator[Top] =
      lzy {
        partGens
          .value
          .productIterator
          .map(_.asInstanceOf[Generator[Top]])
          .reduceOption(_ `combineK` _)
          .getOrElse(Empty())
      }
  }

  def anySumOfShape[T](using mirror: deriving.Mirror.SumOf[T]): AnySumOfShape[T, Tuple.Map[mirror.MirroredElemTypes, Generator]] =
    new AnySumOfShape

  def anyOf[T](using gen: =>Generator[T]): Generator[T] = lzy(gen)

  def anyFromSeq[T](seq: Seq[T]): Generator[T] =
    Selection(Chain.fromSeq(seq))

  def levelIdx(limit: Int = Int.MaxValue): Generator[Int] =
    unfoldLevels(limit = limit)(pure)

  def unfoldLevels[T](limit: Int = Int.MaxValue)(fn: Int => Generator[T]): Generator[T] = {
    require(limit >= 0)
    def impl(level: Int): Generator[T] =
      if(level <= limit) {
        fn(level) | lzy(impl(level + 1))
      } else {
        empty
      }

    impl(level = 0)
  }

  def listOf[T](gen: Generator[T], limit: Int = Int.MaxValue): Generator[List[T]] = {
    require(limit >= 0)
    unfoldLevels(limit = limit)(gen.replicateA)
  }

  def consOf[T](genHd: Generator[T], genTl: Generator[List[T]]): Generator[List[T]] =
    (genHd, genTl).mapN(_ :: _)

  given anyListOf[T](using gen: Generator[T]): Generator[List[T]] = listOf(gen)

  private def tupleMapN[T <: Tuple](genTpl: Tuple.Map[T, Generator]): Generator[T] =
    genTpl
      .productIterator
      .asInstanceOf[Iterator[Generator[?]]]
      .foldRight(pure(EmptyTuple): Generator[? <: Tuple]) { (gen, tplGen) =>
        Applicative[Generator].map2(gen, tplGen)(_ *: _)
      }
      .asInstanceOf[Generator[T]]

  given anyProduct[T](using mirror: deriving.Mirror.ProductOf[T])(using mirror.MirroredElemLabels <:< NonEmptyTuple)(using elemGens: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, Generator]]): Generator[T] =
    lzy {
      tupleMapN(elemGens.value)
        .map { parts =>
          try {
            Some(mirror.fromTuple(parts))
          } catch {
            case _: IllegalArgumentException =>
              None
          }
        }
        .filter(_.nonEmpty)
        .map(_.get)
    }

  given anyEmptyProduct[T](using mirror: deriving.Mirror.ProductOf[T])(using EmptyTuple =:= mirror.MirroredElemTypes): Generator[T] =
    mirror.fromTuple(EmptyTuple).pure

  given anySum[T](using mirror: deriving.Mirror.SumOf[T])(using partGens: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, Generator]]): Generator[T] =
    lzy {
      partGens
        .value
        .productIterator
        .map(_.asInstanceOf[Generator[T]])
        .reduceOption(_ `combineK` _)
        .getOrElse(Empty())
    }

  given anySumK[F[_], T](using mirror: deriving.Mirror.SumOf[T])(using partGensK: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, [TT] =>> Generator[F[TT]]]]): Generator[F[T]] =
    lzy {
      partGensK
        .value
        .productIterator
        .map(_.asInstanceOf[Generator[F[T]]])
        .reduceOption(_ `combineK` _)
        .getOrElse(Empty())
    }
}
