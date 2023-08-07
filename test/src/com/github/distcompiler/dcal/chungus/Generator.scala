package test.com.github.distcompiler.dcal.chungus

import scala.annotation.targetName
import scala.util.Try

import cats.data.{Chain, Ior}
import cats.{Semigroup, Alternative, Eval, Monoid, Foldable, Applicative, MonoidK}
import cats.implicits.given

enum Generator[T] {
  import Generator.*

  case Empty[T]() extends Generator[T]
  case Singleton(value: T)
  case Selection(values: Chain[T])
  case Log(gen: Generator[T], label: String)
  case FreeLazy(genFn: () => Generator[T])
  case Pay(genFn: () => Generator[T])
  //case IncrementCount(key: Counters.Key[T], gen: Generator[T])
  case Filter(self: Generator[T], pred: T => Boolean)
  case Disjunction(left: Generator[T], right: Generator[T])
  case Ap[T, U](transform: Generator[T => U], self: Generator[T]) extends Generator[U]

  def up[U >: T]: Generator[U] =
    this.map(identity)

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

  def ++(using semigroup: Semigroup[T])(other: Generator[T]): Generator[T] =
    this.map2(other)(_ `combine` _)
  
  @targetName("or")
  def |[U](other: Generator[U]): Generator[T | U] =
    this.up.combineK(other.up)
    // (this, other) match {
    //   case (Empty, _) => other
    //   case (_, Empty) => this
    //   case (Singleton(left), Singleton(right)) => Selection(Chain(left, right))
    //   case (Singleton(left), Selection(rights)) => Selection(left +: rights)
    //   case (Selection(lefts), Singleton(right)) => Selection(lefts :+ right)
    //   case (Pay(genL), Pay(genR)) => Pay(genL | genR)
    //   case (Cheapen(genL), Cheapen(genR)) => Cheapen(genL | genR)
    //   case _ => Disjunction(this.up, other.up)
    // }

  def toChecker: Checker.RootChecker[T] =
    Checker.fromGenerator(this)

  def examplesIterator: Iterator[Example[Option[T]]] = {
    final case class Ex[+T](value: T, counters: Counters)

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

    def impl[T](self: Generator[T], counters: Counters): LazyList[() => Iterator[Ex[T]]] =
      self match {
        case Empty() =>
          LazyList.empty
        case Singleton(value) =>
          (() => Iterator.single(Ex(value, counters = counters))) #:: LazyList.empty
        case Selection(values) =>
          (() => values
            .iterator
            .map(Ex(_, counters = counters)))
          #:: LazyList.empty
        case Log(gen, label) =>
          impl(gen, counters)
            .zipWithIndex
            .map {
              case (iterFn, depth) =>
                { () =>
                  println(s"! instantiate $label @ depth $depth")
                  iterFn()
                    .tapEach {
                      case Ex(value, _) =>
                        println(s"  $label @ depth $depth found ")
                        pprint.pprintln(value)
                    }
                }
            }
        case FreeLazy(genFn) =>
          impl(genFn(), counters)
        case Pay(genFn) =>
          (() => Iterator.empty) #:: impl(genFn(), counters)
        case Filter(self, pred) =>
          impl(self, counters)
            .map { iterFn =>
              () => iterFn().filter {
                case Ex(value, _) => pred(value)
              }
            }
        case Disjunction(left, right) =>
          (impl(left, counters) `zipIorLazyLists` impl(right, counters)).map {
            case Ior.Left(leftIterFn) => leftIterFn
            case Ior.Right(rightIterFn) => rightIterFn
            case Ior.Both(leftIterFn, rightIterFn) =>
              () => leftIterFn() ++ rightIterFn()
          }
        case ap: Ap[tt, T] =>
          val selfIterFns = impl(ap.self, counters)
          val transIterFns = impl(ap.transform, counters)

          def incSelf(selfIter: Iterator[Ex[tt]], depth: Int): Iterator[Iterator[Ex[T]]] =
            selfIter.flatMap {
              case Ex(value, counters) =>
                transIterFns
                  .iterator
                  .take(depth + 1)
                  .map { transIterFn =>
                    transIterFn().map {
                      case Ex(transform, counters) =>
                        Ex(transform(value), counters) 
                    }
                  }
            }

          def incTrans(transIter: Iterator[Ex[tt => T]], depth: Int): Iterator[Iterator[Ex[T]]] =
            transIter.flatMap {
              case Ex(transform, counters) =>
                selfIterFns
                  .iterator
                  .take(depth + 1)
                  .map { selfIterFn =>
                    selfIterFn().map {
                      case Ex(value, counters) =>
                        Ex(transform(value), counters) 
                    }
                  }
            }

          (selfIterFns `zipIorLazyLists` transIterFns)
            .zipWithIndex
            .map {
              case (Ior.Left(selfIterFn), depth) =>
                () => incSelf(selfIterFn(), depth).flatten
              case (Ior.Right(transIterFn), depth) =>
                () => incTrans(transIterFn(), depth).flatten
              case (Ior.Both(selfIterFn, transIterFn), depth) =>
                { () =>
                  (incSelf(selfIterFn(), depth) `zipIorIterators` incTrans(transIterFn(), depth - 1))
                    .flatMap {
                      case Ior.Both(selfIter, otherIter) => selfIter ++ otherIter
                      case Ior.Left(selfIter) => selfIter
                      case Ior.Right(otherIter) => otherIter
                    }
                }
            }
      }
      
    impl(this, counters = Counters.init)
      .iterator
      .zipWithIndex
      .flatMap {
        case (iterFn, depth) =>
          iterFn().map {
            case Ex(value, counters) =>
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

  given generatorAlternative: Alternative[Generator] with {
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

  given generatorMonoid[T]: Monoid[Generator[T]] = generatorAlternative.algebra

  def empty[T]: Generator[T] = Empty()

  def pure[T](value: T): Generator[T] = value.pure

  def freeLzy_![T](gen: =>Generator[T]): Generator[T] = {
    lazy val lzyGen = gen
    FreeLazy(() => lzyGen)
  }

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

  def unfold[T](init: Generator[T], limit: Int = Int.MaxValue)(fn: Generator[T] => Generator[T]): Generator[T] = {
    require(limit >= 0)
    def impl(gen: Generator[T], level: Int): Generator[T] =
      if(level <= limit) {
        gen
        | lzy(impl(fn(gen), level = level + 1))
      } else {
        empty
      }

    impl(init, level = 0)
  }

  def listOf[T](gen: Generator[T], limit: Int = Int.MaxValue): Generator[List[T]] = {
    require(limit >= 0)
    unfold(Nil.pure, limit = limit)(consOf(gen, _))
  }

  def consOf[T](genHd: Generator[T], genTl: Generator[List[T]]): Generator[List[T]] =
    (genHd, genTl).mapN(_ :: _)

  given anyListOf[T](using gen: Generator[T]): Generator[List[T]] = listOf(gen)

  // def count[T](key: Counters.Key[T])(gen: Generator[T]): Generator[T] =
  //   Generator.IncrementCount(key, gen)

  // def rateLimit[T](key: Counters.Key[T], limit: Int)(gen: Generator[T]): Generator[T] =
  //   readCounters.flatMap { counters =>
  //     if(counters.read(key) < limit) {
  //       count(key)(gen).flatMap(_ => lzy(rateLimit(key, limit = limit)(gen)))
  //     } else {
  //       none.up
  //     }
  //   }

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
}
