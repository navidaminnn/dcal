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
      case ReadCounters => FlatMap(this, fn)
      case Singleton(value) => fn(value)
      case Selection(values) =>
        values.iterator
          .map(fn)
          .reduce(_ | _)
      case Force(_) | Lazy(_) => FlatMap(this, fn)
      case Pay(gen) => Pay(gen.flatMap(fn))
      case IncrementCount(_, _) => FlatMap(this, fn)
      case Disjunction(left, right) =>
        left.flatMap(fn) | right.flatMap(fn)
      case FlatMap(_, _) => FlatMap(this, fn)
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
      case _ => Disjunction(this.up, other.up)
    }

  def checkWith(checker: Checker[T]): Unit =
    checker(this)

  def computeResultsForRound(round: Int): Iterator[Option[T]] = {
    enum Step[+T] {
      case Done
      case Progress(value: T, counters: Counters, nextStep: Eval[Step[T]])
    }

    def impl[T](self: Generator[T], counters: Counters): Eval[Step[T]] = {
      self match {
        case Empty => Eval.now(Step.Done)
        case ReadCounters =>
          Eval.now(Step.Progress(counters, counters, Eval.now(Step.Done)))
        case Singleton(value) =>
          Eval.now(Step.Progress(value, counters, Eval.now(Step.Done)))
        case Selection(values) =>
          def perpetuate(values: Chain[T]): Eval[Step[T]] =
            values.uncons match {
              case None => Eval.now(Step.Done)
              case Some(value, restValues) =>
                Eval.now(Step.Progress(value, counters, Eval.defer(perpetuate(restValues))))
            }

          perpetuate(values)
        case Force(gen) =>
          def perpetuate(step: Eval[Step[T]]): Eval[Step[T]] =
            step.map {
              case Step.Done => Step.Done
              case Step.Progress(valueOpt, countersToReset, nextStep) =>
                Step.Progress(valueOpt, countersToReset.resetBudget(counters.budget), perpetuate(nextStep))
            }

          perpetuate(impl(gen, counters.resetBudget(Int.MaxValue)))
        case Pay(gen) =>
          if(counters.budget == 0) {
            Eval.now(Step.Done)
          } else {
            Eval.defer(impl(gen, counters.payOne))
          }
        case Lazy(genFn) =>
          Eval.defer(impl(genFn(), counters))
        case IncrementCount(key, gen) =>
          Eval.defer(impl(gen, counters.countOne(key)))
        case Disjunction(left, right) =>
          def attachRight(step: Eval[Step[T]]): Eval[Step[T]] =
            step.flatMap {
              case Step.Done =>
                Eval.defer(impl(right, counters))
              case Step.Progress(value, counters, nextStep) =>
                Eval.now(Step.Progress(value, counters, attachRight(nextStep)))
            }

          attachRight(Eval.defer(impl(left, counters)))
        case fm: FlatMap[tt, T] =>
          val self = fm.self
          val fn = fm.fn
          def perpetuateOuter(step: Eval[Step[tt]]): Eval[Step[T]] =
            step.flatMap {
              case Step.Done => Eval.now(Step.Done)
              case Step.Progress(value, counters, nextStep) =>
                def perpetuateInner(step: Eval[Step[T]]): Eval[Step[T]] =
                  step.flatMap {
                    case Step.Done =>
                      perpetuateOuter(nextStep)
                    case Step.Progress(value, counters, nextStep) =>
                      Eval.now(Step.Progress(value, counters, perpetuateInner(nextStep)))
                  }

                perpetuateInner(Eval.defer(impl(fn(value), counters)))
            }

          perpetuateOuter(Eval.defer(impl(self, counters)))
      }
    }

    Iterator.unfold(impl(this, Counters(budget = round))) { nextStep =>
      nextStep.value match {
        case Step.Done => None
        case Step.Progress(value, counters, nextStep) =>
          Some((if(counters.budget == 0) Some(value) else None, nextStep))
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

  def unfold[T](init: Generator[T], limit: Int = Int.MaxValue)(grow: T => Generator[T]): Generator[T] = {
    require(limit >= 0)
    init.flatMap { init =>
      if(limit == 0) {
        free_!(init)
      } else {
        free_!(init)
        | unfold(lzy(costOne(grow(init))), limit = limit - 1)(grow)
      }
    }
  }

  def listOf[T](generator: Generator[T], limit: Int = Int.MaxValue): Generator[List[T]] =
    unfold(one(Nil), limit = limit) { tail =>
      generator.map(_ :: tail)
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
