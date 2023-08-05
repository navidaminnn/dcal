package test.com.github.distcompiler.dcal.chungus

import scala.annotation.targetName
import scala.util.Try

import cats.data.Chain
import cats.{Semigroup, StackSafeMonad, Eval, Monoid}
import cats.implicits.given
import cats.Foldable
import scala.reflect.ClassTag

enum Generator[T] {
  import Generator.*
  this match {
    case Selection(values) =>
      assert(values.nonEmpty)
    case _ =>
  }

  case Empty extends Generator[Nothing]
  case ReadCounters extends Generator[Counters]
  case Singleton(value: T)
  case Selection(values: Chain[T])
  case Force(gen: Generator[T])
  case Pay(gen: Generator[T])
  case Cheapen(gen: Generator[T])
  case Lazy(genFn: () => Generator[T])
  case IncrementCount(key: Counters.Key[T], gen: Generator[T])
  case Disjunction(left: Generator[T], right: Generator[T])
  case FlatMap[T, U](self: Generator[T], fn: T => Generator[U]) extends Generator[U]

  def isDefinitelyEmpty: Boolean =
    this match {
      case Empty => true
      case _ => false
    }

  def up[U >: T]: Generator[U] =
    this.asInstanceOf[Generator[U]]

  def map[U](fn: T => U): Generator[U] =
    this.flatMap(value => free_!(fn(value)))

  def filter(fn: T => Boolean): Generator[T] =
    this.flatMap { value =>
      if(fn(value)) {
        free_!(value)
      } else {
        none.up
      }
    }

  def ++(using semigroup: Semigroup[T])(other: Generator[T]): Generator[T] =
    for {
      left <- this
      right <- other
    } yield semigroup.combine(left, right)

  def flatMap[U](fn: T => Generator[U]): Generator[U] =
    this match {
      case Empty => Empty.up
      case Singleton(value) => fn(value)
      case Selection(values) =>
        values.iterator
          .map(fn)
          .reduce(_ | _)
      case Disjunction(left, right) =>
        left.flatMap(fn) | right.flatMap(fn)
      case _ => FlatMap(this, fn)
    }

  def force: Generator[T] =
    Force(this)
  
  @targetName("or")
  def |[U](other: Generator[U]): Generator[T | U] =
    (this, other) match {
      case (Empty, _) => other
      case (_, Empty) => this
      case (Singleton(left), Singleton(right)) => Selection(Chain(left, right))
      case (Singleton(left), Selection(rights)) => Selection(left +: rights)
      case (Selection(lefts), Singleton(right)) => Selection(lefts :+ right)
      case (Pay(genL), Pay(genR)) => Pay(genL | genR)
      case (Cheapen(genL), Cheapen(genR)) => Cheapen(genL | genR)
      case _ => Disjunction(this.up, other.up)
    }

  def toChecker: Checker.RootChecker[T] =
    Checker.fromGenerator(this)

  def computeResultsForDepth(depth: Int): Iterator[Option[T]] = {
    enum Step[+T] {
      case Done
      case Progress(value: T, maxLevelReached: Int, counters: Counters, nextStep: Eval[Step[T]])
    }

    def impl[T](self: Generator[T], level: Int, counters: Counters)(using maxLevel: Int): Eval[Step[T]] = {
      self match {
        case Empty => Eval.now(Step.Done)
        case ReadCounters =>
          Eval.now(Step.Progress(counters, level, counters, Eval.now(Step.Done)))
        case Singleton(value) =>
          Eval.now(Step.Progress(value, level, counters, Eval.now(Step.Done)))
        case Selection(values) =>
          def perpetuate(values: Chain[T]): Eval[Step[T]] =
            values.uncons match {
              case None => Eval.now(Step.Done)
              case Some(value, restValues) =>
                Eval.now(Step.Progress(value, level, counters, Eval.defer(perpetuate(restValues))))
            }

          perpetuate(values)
        case Force(gen) =>
          def perpetuate(step: Eval[Step[T]]): Eval[Step[T]] =
            step.map {
              case Step.Done => Step.Done
              case Step.Progress(value, _, counters, nextStep) =>
                Step.Progress(value, level, counters, perpetuate(nextStep))
            }

          perpetuate(impl(gen, level, counters)(using Int.MaxValue))
        case Pay(gen) =>
          if(level >= maxLevel) {
            Eval.now(Step.Done)
          } else {
            Eval.defer(impl(gen, level + 1, counters))
          }
        case Cheapen(gen) =>
          Eval.defer(impl(gen, level - 1, counters))
        case Lazy(genFn) =>
          Eval.defer(impl(genFn(), level, counters))
        case IncrementCount(key, gen) =>
          Eval.defer(impl(gen, level, counters.countOne(key)))
        case Disjunction(left, right) =>
          def attachRight(step: Eval[Step[T]]): Eval[Step[T]] =
            step.flatMap {
              case Step.Done =>
                Eval.defer(impl(right, level, counters))
              case Step.Progress(value, maxLevelReached, counters, nextStep) =>
                Eval.now(Step.Progress(value, maxLevelReached, counters, attachRight(nextStep)))
            }

          attachRight(Eval.defer(impl(left, level, counters)))
        case fm: FlatMap[tt, T] =>
          val self = fm.self
          val fn = fm.fn
          def perpetuateOuter(step: Eval[Step[tt]]): Eval[Step[T]] =
            step.flatMap {
              case Step.Done => Eval.now(Step.Done)
              case Step.Progress(value, maxLevelReached1, counters, nextStep) =>
                def perpetuateInner(step: Eval[Step[T]]): Eval[Step[T]] =
                  step.flatMap {
                    case Step.Done =>
                      perpetuateOuter(nextStep)
                    case Step.Progress(value, maxLevelReached2, counters, nextStep) =>
                      Eval.now(Step.Progress(value, math.max(maxLevelReached1, maxLevelReached2), counters, perpetuateInner(nextStep)))
                  }

                perpetuateInner(Eval.defer(impl(fn(value), level, counters)))
            }

          perpetuateOuter(Eval.defer(impl(self, level, counters)))
      }
    }

    Iterator.unfold(impl(this, 0, Counters.init)(using depth)) { nextStep =>
      nextStep.value match {
        case Step.Done => None
        case Step.Progress(value, maxLevelReached, counters, nextStep) =>
          if(maxLevelReached == depth) {
            Some((Some(value), nextStep))
          } else {
            Some((None, nextStep))
          }
      }
    }
  }
}

object Generator {
  given generatorMonad: StackSafeMonad[Generator] with {
    override def pure[A](x: A): Generator[A] = free_!(x)

    override def flatMap[A, B](fa: Generator[A])(f: A => Generator[B]): Generator[B] = fa.flatMap(f)
  }

  given generatorMonoid[T]: Monoid[Generator[T]] with {
    override def empty: Generator[T] = none.up

    override def combine(x: Generator[T], y: Generator[T]): Generator[T] = x | y
  }

  def none: Generator[Nothing] =
    Generator.Empty

  def readCounters: Generator[Counters] =
    Generator.ReadCounters

  def one[T](value: T): Generator[T] =
    costOne(free_!(value))

  def cheapen_![T](gen: Generator[T]): Generator[T] =
    Generator.Cheapen(gen)

  def free_![T](value: T): Generator[T] =
    Generator.Singleton(value)

  def costOne[T](gen: Generator[T]): Generator[T] =
    Generator.Pay(gen)

  def lzy[T](gen: =>Generator[T]): Generator[T] = {
    lazy val lzyGen = gen
    Generator.Lazy(() => gen)
  }

  def anyOf[T](using gen: =>Generator[T]): Generator[T] = lzy(gen)

  def chooseAny[T, F[_] : Foldable](ft: F[T]): Generator[T] =
    ft.foldMap(one)

  def unfold[T](init: T, limit: Int = Int.MaxValue)(grow: T => Generator[T]): Generator[T] = {
    require(limit >= 0)
    def impl(prev: T, level: Int): Generator[T] =
      if(level >= limit) {
        none.up
      } else {
        free_!(prev)
        | costOne {
          grow(prev).flatMap { curr =>
            free_!(curr)
            | impl(curr, level + 1)
          }
        }
      }

    impl(init, 0)
  }

  def listOf[T](generator: Generator[T], limit: Int = Int.MaxValue): Generator[List[T]] =
    unfold(Nil, limit = limit) { tail =>
      cheapen_!(generator).map(_ :: tail)
    }

  def count[T](key: Counters.Key[T])(gen: Generator[T]): Generator[T] =
    Generator.IncrementCount(key, gen)

  def rateLimit[T](key: Counters.Key[T], limit: Int)(gen: Generator[T]): Generator[T] =
    readCounters.flatMap { counters =>
      if(counters.read(key) < limit) {
        count(key)(gen)
      } else {
        none.up
      }
    }

  given anyProductEmptyTuple: Generator[EmptyTuple] = free_!(EmptyTuple)
  given anyProductTupleCons[T, Rest <: Tuple](using tGen: =>Generator[T], restGen: =>Generator[Rest]): Generator[T *: Rest] =
    for {
      t <- lzy(tGen)
      rest <- lzy(restGen)
    } yield t *: rest

  given anyProduct[T](using mirror: deriving.Mirror.ProductOf[T])(using mirror.MirroredElemTypes <:< NonEmptyTuple)(using genParts: =>Generator[mirror.MirroredElemTypes]): Generator[T] =
    lzy { 
      costOne(genParts)
        .map { tuple =>
          Try(mirror.fromTuple(tuple))
        }
        .flatMap { result =>
          result
            .map(value => free_!(value))
            .recover {
              case _: IllegalArgumentException =>
                none.up
            }
            .get
        }
    }

  given anyEmptyProduct[T](using mirror: deriving.Mirror.ProductOf[T])(using EmptyTuple =:= mirror.MirroredElemTypes): Generator[T] =
    one(mirror.fromTuple(EmptyTuple))

  given anySumTuple1[T](using gen: Generator[T]): Tuple1[Generator[T]] =
    Tuple1(gen)
  given anySumTupleCons[T, Rest <: Tuple](using ev: Tuple.InverseMap[Rest, Generator] <:< Tuple)(using tGen: Generator[T], rest: Rest): (Generator[T] *: Rest) =
    tGen *: rest

  given anySum[T](using mirror: deriving.Mirror.SumOf[T])(using partGens: =>Tuple.Map[mirror.MirroredElemTypes, Generator]): Generator[T] =
    lzy {
      partGens
        .productIterator
        .foldLeft(none.up: Generator[T]) { (acc, gen) =>
          acc | gen.asInstanceOf[Generator[T]]
        }
    }

  given anyListOf[T](using gen: Generator[T]): Generator[List[T]] = listOf(gen)
}
