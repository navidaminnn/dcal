package test.com.github.distcompiler.dcal.chungus

import com.github.distcompiler.dcal.util.{SummonTuple, EvalList}

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

    def impl[T](self: Generator[T]): EvalList[EvalList[T]] =
      self match {
        case Empty() =>
          EvalList.empty
        case Singleton(value) =>
          EvalList.single(EvalList.single(value))
        case Selection(values) =>
          EvalList.single(EvalList.fromIterable(values.toIterable))
        case Log(gen, label) =>
          impl(gen)
            .zipWithIndex
            .map {
              case (iterFn, depth) =>
                EvalList.defer {
                  println(s"! instantiate $label @ depth $depth")
                  iterFn
                    .tapEach { value =>
                      print(s"  $label @ depth $depth found ")
                      pprint.pprintln(value)
                    }
                }
            }
        case Pay(genFn) =>
          EvalList.empty +: EvalList.defer(impl(genFn()))
        case Filter(self, pred) =>
          impl(self)
            .map(_.filter(pred))
        case Disjunction(left, right) =>
          (impl(left) `zipIor` impl(right))
            .map(_.fold(identity, identity, _ ++ _))
        case ap: Ap[tt, T] =>
          val selfIterFns = impl(ap.self).memoize
          val transIterFns = impl(ap.transform).memoize

          def incSelf(selfIter: EvalList[tt], depth: Int): EvalList[EvalList[T]] =
            selfIter.flatMap { value =>
              transIterFns
                .take(depth + 1)
                .map { transIterFn =>
                  transIterFn.map(_.apply(value))
                }
            }

          def incTrans(transIter: EvalList[tt => T], depth: Int): EvalList[EvalList[T]] =
            transIter.flatMap { transform =>
              selfIterFns
                .take(depth + 1)
                .map { selfIterFn =>
                  selfIterFn.map(transform)
                }
            }

          (selfIterFns `zipIor` transIterFns)
            .zipWithIndex
            .map {
              case (Ior.Left(selfIterFn), depth) =>
                incSelf(selfIterFn, depth).flatten
              case (Ior.Right(transIterFn), depth) =>
                incTrans(transIterFn, depth).flatten
              case (Ior.Both(selfIterFn, transIterFn), depth) =>
                (incSelf(selfIterFn, depth) `zipIor` incTrans(transIterFn, depth - 1))
                  .flatMap(_.fold(identity, identity, _ ++ _))
            }
        case andThen: AndThen[tt, T] =>
          val self = andThen.self
          val fn = andThen.fn

          def weave(selfElems: EvalList[EvalList[tt]], existingDepthList: EvalList[EvalList[T]]): EvalList[EvalList[T]] =
            selfElems
              .asEvalCell
              .flatMap {
                case EvalList.Cell.Nil => existingDepthList.asEvalCell
                case selfElems @ (EvalList.Cell.Single(_) | EvalList.Cell.Cons(_, _)) =>
                  val (iterFn, nextSelfElems) =
                    selfElems match {
                      case EvalList.Cell.Single(value) => (value, EvalList.empty)
                      case EvalList.Cell.Cons(head, tail) => (head, tail)
                    }

                  existingDepthList
                    .asEvalCell
                    .map {
                      case EvalList.Cell.Nil => (EvalList.empty, EvalList.empty)
                      case EvalList.Cell.Single(value) => (value, EvalList.empty)
                      case EvalList.Cell.Cons(head, tail) => (head, tail)
                    }
                    .flatMap {
                      case (currentExistingIter, nextExistingDepthList) =>
                        var nextIterBufferUsed = false
                        var reachedIdx = -1
                        val nextIterBuffer = mutable.ListBuffer.empty[EvalList[EvalList[T]]]

                        val currentAdditionalIter: EvalList[T] =
                          iterFn
                            .zipWithIndex
                            .flatMap {
                              case (value, idx) =>
                                impl(fn(value))
                                  .asEvalCell
                                  .map {
                                    case EvalList.Cell.Nil => EvalList.empty
                                    case EvalList.Cell.Single(value) => value
                                    case EvalList.Cell.Cons(iter, nextIters) =>
                                      // note: do not under any circumstance evaluate nextIters!
                                      // doing that will make the assert below fail, because it breaks
                                      // the invariant: we will always exhaust shallower iters before
                                      // asking for deeper ones
                                      if(reachedIdx < idx) {
                                        assert(!nextIterBufferUsed)
                                        nextIterBuffer += nextIters
                                      }
                                      reachedIdx = idx `max` reachedIdx

                                      iter
                                  }
                                  .flatMap(_.asEvalCell)
                                  .asEvalList
                            }

                        ((currentExistingIter ++ currentAdditionalIter) +: {
                          val nextDepthList =
                            EvalList.defer {
                              nextIterBufferUsed = true
                              nextIterBuffer
                                .foldLeft(nextExistingDepthList)({ (acc, list) =>
                                  (acc `zipIor` list).map(_.fold(identity, identity, _ ++ _))
                                })
                            }
                            .memoize
                          
                          weave(nextSelfElems, nextDepthList)
                        })
                          .asEvalCell
                    }
              }
              .asEvalList

          weave(impl(self), EvalList.empty)
      }
      
    impl(this)
      .iterator
      .zipWithIndex
      .flatMap {
        case (iterFn, depth) =>
          iterFn.iterator.map { value =>
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

  type TupleExcluding[T, Tpl <: Tuple] <: Tuple = Tpl match {
    case (T *: tl) => tl
    case (hd *: tl) => hd *: TupleExcluding[T, tl]
  }

  final class AnySumOfShape[Top, Tpl <: Tuple] {
    def excluding[TT]: AnySumOfShape[Top, TupleExcluding[Generator[TT], Tpl]] = new AnySumOfShape

    def summon(using partGens: =>SummonTuple[Tpl]): Generator[Top] =
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

  given anyAlternative[F[_], T](using Alternative[F])(using gen: Generator[T]): Generator[F[T]] =
    unfoldLevels() { level =>
      gen.replicateA(level).map(_.foldMapK(_.pure))
    }

  private def tupleMapN[T <: Tuple](genTpl: Tuple.Map[T, Generator]): Generator[T] =
    genTpl
      .productIterator
      .asInstanceOf[Iterator[Generator[?]]]
      .foldRight(pure(EmptyTuple): Generator[? <: Tuple]) { (gen, tplGen) =>
        Applicative[Generator].map2(gen, tplGen)(_ *: _)
      }
      .asInstanceOf[Generator[T]]

  given anyProduct[T](using mirror: deriving.Mirror.ProductOf[T])(using mirror.MirroredElemLabels <:< NonEmptyTuple)(using elemGens: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, Generator]]): Generator[T] =
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

  given anySum[T](using mirror: deriving.Mirror.SumOf[T])(using partGens: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, Generator]]): Generator[T] =
    lzy {
      partGens
        .value
        .productIterator
        .map(_.asInstanceOf[Generator[T]])
        .reduceOption(_ `combineK` _)
        .getOrElse(Empty())
    }

  given anySumK[F[_], T](using mirror: deriving.Mirror.SumOf[T])(using partGensK: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [TT] =>> Generator[F[TT]]]]): Generator[F[T]] =
    lzy {
      partGensK
        .value
        .productIterator
        .map(_.asInstanceOf[Generator[F[T]]])
        .reduceOption(_ `combineK` _)
        .getOrElse(Empty())
    }
}
