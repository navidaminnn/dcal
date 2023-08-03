package test.com.github.distcompiler.dcal.chungus

import scala.annotation.targetName
import scala.util.Try

import cats.data.Chain
import cats.{Eval, Semigroup}
import cats.implicits.given

enum Generator[T] {
  import Generator.*
  this match {
    case Selection(values) =>
      assert(values.nonEmpty)
    case _ =>
  }

  case Empty extends Generator[Nothing]
  case Singleton(value: T)
  case Selection(values: Chain[T])
  case Force(gen: Generator[T])
  case Pay(gen: Generator[T])
  case Lazy(genFn: () => Generator[T])
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
      case Force(_) | Lazy(_) => FlatMap(this, fn)
      case Pay(gen) => Pay(gen.flatMap(fn))
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
    def impl[T](self: Generator[T], budget: Int): Iterator[(T, Int)] = {
      require(budget >= 0)
      self match {
        case Empty => Iterator.empty
        case Singleton(value) => Iterator.single((value, budget))
        case Selection(values) => values.iterator.map((_, budget))
        case Force(gen) =>
          impl(gen, budget = Int.MaxValue).map {
            case (value, _) => (value, budget)
          }
        case Pay(gen) if budget == 0 => Iterator.empty
        case Pay(gen) => impl(gen, budget = budget - 1)
        case Lazy(genFn) => impl(genFn(), budget)
        case Disjunction(left, right) =>
          impl(left, budget) ++ impl(right, budget)
        case FlatMap(self, fn) =>
          impl(self, budget).flatMap {
            case (value, budget) => impl(fn(value), budget)
          }
      }
    }
    
    impl(this, budget = round)
      .map {
        case (value, 0) => Some(value)
        case _ => None
      }
  }
}

object Generator {
  def none: Generator[Nothing] =
    Generator.Empty

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

  given tupleGeneratorEmpty: Generator[EmptyTuple] = free_!(EmptyTuple)
  given tupleGeneratorCons[T, Rest <: Tuple](using tGen: =>Generator[T], restGen: =>Generator[Rest]): Generator[T *: Rest] =
    for {
      t <- lzy(tGen)
      rest <- lzy(restGen)
    } yield t *: rest

  given productGenerator[T](using mirror: deriving.Mirror.ProductOf[T])(using mirror.MirroredElemTypes <:< NonEmptyTuple)(using genParts: =>Generator[mirror.MirroredElemTypes]): Generator[T] =
    costOne(lzy(genParts))
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

  given productGeneratorSingleton[T](using mirror: deriving.Mirror.ProductOf[T])(using EmptyTuple =:= mirror.MirroredElemTypes): Generator[T] =
    one(mirror.fromTuple(EmptyTuple))

  given generatorTupleSingleton[T](using gen: Generator[T]): Tuple1[Generator[T]] =
    Tuple1(gen)
  given generatorTupleCons[T, Rest <: Tuple](using ev: Tuple.InverseMap[Rest, Generator] <:< Tuple)(using tGen: Generator[T], rest: Rest): (Generator[T] *: Rest) =
    tGen *: rest

  given sumGenerator[T](using mirror: deriving.Mirror.SumOf[T])(using partGens: =>Tuple.Map[mirror.MirroredElemTypes, Generator]): Generator[T] =
    lzy {
      partGens
        .productIterator
        .foldLeft(none.up: Generator[T]) { (acc, gen) =>
          acc | gen.asInstanceOf[Generator[T]]
        }
    }

  given listOfGenerator[T](using gen: Generator[T]): Generator[List[T]] = listOf(gen)

  def anyOf[T](using gen: =>Generator[T]): Generator[T] = lzy(gen)

  def chooseAny[T](iterable: Iterable[T]): Generator[T] =
    costOne {
      iterable.iterator
        .map(free_!)
        .reduceOption(_ | _)
        .getOrElse(none.up)
    }

  def listOf[T](generator: Generator[T], limit: Int = Int.MaxValue): Generator[List[T]] = {
    require(limit >= 0)
    if(limit == 0) {
      none.up
    } else {
      one(Nil)
      | costOne {
          for {
            head <- generator
            tail <- lzy(listOf(generator, limit = limit - 1))
          } yield head :: tail
        }
    }
  }

  trait ChainCatable[T] {
    def elems: Generator[Chain[T]]
  }
  object ChainCatable {
    def apply[T](gen: Generator[Chain[T]]): ChainCatable[T] =
      new ChainCatable[T] {
        override def elems: Generator[Chain[T]] = gen
      }

    given oneChain[T]: Conversion[Chain[T], ChainCatable[T]] = one

    given singleton[T]: Conversion[T, ChainCatable[T]] = Chain.one

    given iterableOnceSplay[U, T](using conv: Conversion[U, ChainCatable[T]]): Conversion[IterableOnce[U], ChainCatable[T]] with {
      override def apply(x: IterableOnce[U]): ChainCatable[T] =
        chainCat(x.iterator.map(conv).toSeq*)
    }

    given chainChain[T]: Conversion[Generator[Chain[T]], ChainCatable[T]] = ChainCatable(_)
  }

  def chainCat[T](catables: ChainCatable[T]*): Generator[Chain[T]] =
    catables.foldLeft(one(Chain.nil): Generator[Chain[T]]) { (acc, catable) =>
      acc.flatMap { prefix =>
        catable.elems.map(prefix ++ _)
      }
    }
}
