package test.com.github.distcompiler.dcal.chungus

import scala.annotation.targetName
import scala.util.Try

import cats.data.Chain
import cats.{Semigroup, Alternative, Eval, Monoid, Foldable, Applicative, MonoidK}
import cats.implicits.given

enum Generator[T] {
  import Generator.*
  this match {
    case Selection(values) =>
      assert(values.nonEmpty)
    case _ =>
  }

  case Empty[T]() extends Generator[T]
  case Singleton(value: T)
  case Selection(values: Chain[T])
  case Force(gen: Generator[T])
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

  def examplesIterator: Iterator[Example[T]] = {
    var haveLeftovers = false

    final case class Ex[+T](value: T, maxDepth: Int, counters: Counters)

    def impl[T](self: Generator[T], counters: Counters, depth: Int, depthCap: Int): Iterator[Ex[T]] =
      self match {
        case Empty() =>
          Iterator.empty
        case Singleton(value) =>
          Iterator.single(Ex(value, maxDepth = depth, counters = counters ))
        case Selection(values) =>
          values
            .iterator
            .map(Ex(_, maxDepth = depth, counters = counters ))
        case Force(gen) =>
          impl(gen, counters, 0, Int.MaxValue)
            .map {
              case Ex(value, _, counters) =>
                Ex(value, maxDepth = depth, counters = counters)
            }
        case FreeLazy(genFn) =>
          impl(genFn(), counters, depth, depthCap)
        case Pay(genFn) =>
          if(depth == depthCap) {
            haveLeftovers = true
            Iterator.empty
          } else {
            impl(genFn(), counters, depth + 1, depthCap)
          }
        case Filter(self, pred) =>
          impl(self, counters, depth, depthCap)
            .filter {
              case Ex(value, _, _) => pred(value)
            }
        case Disjunction(left, right) =>
          val leftIter = impl(left, counters, depth, depthCap).buffered
          val rightIter = impl(right, counters, depth, depthCap).buffered
          
          // try to see smaller before bigger
          new Iterator[Ex[T]] {
            override def hasNext: Boolean = leftIter.hasNext || rightIter.hasNext
            override def next(): Ex[T] =
              if(leftIter.hasNext && rightIter.hasNext) {
                if(leftIter.head.maxDepth <= rightIter.head.maxDepth) {
                  leftIter.next()
                } else {
                  rightIter.next()
                }
              } else if(leftIter.hasNext) {
                leftIter.next()
              } else {
                rightIter.next()
              }
          }
        case Ap(transform, self) =>
          impl(self, counters, depth, depthCap)
            .flatMap {
              case Ex(value, maxDepth1, counters) =>
                impl(transform, counters, depth, depthCap)
                  .map {
                    case Ex(fn, maxDepth2, counters) =>
                      Ex(fn(value), maxDepth1 `max` maxDepth2, counters) 
                  }
            }
      }
      
    Iterator.from(0)
      .map { depthCap =>
        if(depthCap == 0 || haveLeftovers) {
          haveLeftovers = false
          impl(this, counters = Counters.init, depth = 0, depthCap = depthCap)
            .filter(_.maxDepth == depthCap)
            .map {
              case Ex(value, maxDepth, counters) =>
                Example(value, maxDepth)
            }
            .some
        } else {
          None
        }
      }
      .takeWhile(_.nonEmpty)
      .flatMap(_.iterator.flatten)
  }
}

object Generator {
  import Generator.*

  final case class Example[T](value: T, maxDepth: Int)

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

  def empty[T]: Generator[T] = Empty()

  def pure[T](value: T): Generator[T] = value.pure

  def force_![T](gen: Generator[T]): Generator[T] = Force(gen)

  def freeLzy_![T](gen: =>Generator[T]): Generator[T] = {
    lazy val lzyGen = gen
    FreeLazy(() => lzyGen)
  }

  def lzy[T](gen: =>Generator[T]): Generator[T] = {
    lazy val lzyGen = gen
    Pay(() => lzyGen)
  }

  def anyOf[T](using gen: =>Generator[T]): Generator[T] = lzy(gen)

  def anyFromSeq[T](seq: Seq[T]): Generator[T] =
    Selection(Chain.fromSeq(seq))

  def unfold[T](levelFn: Int => Option[Generator[T]]): Generator[T] = {
    def impl(level: Int): Generator[T] =
      levelFn(level) match {
        case None => Empty()
        case Some(gen) =>
          gen
          | lzy(impl(level + 1))
      }

    impl(0)
  }

  def listOf[T](gen: Generator[T], limit: Int = Int.MaxValue): Generator[List[T]] = {
    require(limit >= 0)
    unfold {
      case idx if idx <= limit =>
        Some(Applicative[Generator].replicateA(idx, gen))
      case _ => None
    }
  }

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

  given anyProductEmptyTuple: Generator[EmptyTuple] = EmptyTuple.pure
  given anyProductTupleCons[T, Rest <: Tuple](using tGen: =>Generator[T], restGen: =>Generator[Rest]): Generator[T *: Rest] =
    Applicative[Generator].map2(freeLzy_!(tGen), freeLzy_!(restGen))(_ *: _)

  given anyProduct[T](using mirror: deriving.Mirror.ProductOf[T])(using mirror.MirroredElemTypes <:< NonEmptyTuple)(using genParts: =>Generator[mirror.MirroredElemTypes]): Generator[T] =
    lzy {
      genParts
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

  given anySumTuple1[T](using gen: Generator[T]): Tuple1[Generator[T]] =
    Tuple1(gen)
  given anySumTupleCons[T, Rest <: Tuple](using ev: Tuple.InverseMap[Rest, Generator] <:< Tuple)(using tGen: Generator[T], rest: Rest): (Generator[T] *: Rest) =
    tGen *: rest

  given anySum[T](using mirror: deriving.Mirror.SumOf[T])(using partGens: =>Tuple.Map[mirror.MirroredElemTypes, Generator]): Generator[T] =
    lzy {
      partGens
        .productIterator
        .map(_.asInstanceOf[Generator[T]])
        .reduceOption(_ `combineK` _)
        .getOrElse(Empty())
    }
}
