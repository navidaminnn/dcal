package test.com.github.distcompiler.dcal.chungus

import scala.annotation.targetName
import scala.util.Try

import cats.data.Chain
import cats.{Eval, Semigroup}
import cats.implicits.given

enum Generator[T] {
  import Generator.*

  case Empty extends Generator[Nothing]
  case Singleton(value: T)
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
      case Force(_) | Lazy(_) => FlatMap(this, fn)
      case Pay(gen) => Pay(gen.flatMap(fn))
      case Disjunction(left, right) =>
        left.flatMap(fn) | right.flatMap(fn)
      case FlatMap(self, prevFn) =>
        FlatMap(self, prevFn.andThen(_.flatMap(fn)))
    }

  def force: Generator[T] =
    Force(this)
  
  @targetName("or")
  def |[U](other: Generator[U]): Generator[T | U] =
    (this, other) match {
      case (Empty, _) => other
      case (_, Empty) => this
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

  def unconsRound: (Iterator[Option[T]], Generator[T]) = {
    enum State {
      case Unpaid
      case HavePaid
      case Forced
    }

    def impl[T](self: Generator[T], state: State): Eval[(LazyList[(T, State)], Generator[T])] =
      self match {
        case Empty => Eval.now((LazyList.empty, self))
        case Singleton(value) =>
          Eval.now(((value, state) #:: LazyList.empty, self))
        case Force(gen) =>
          val origState = state
          impl(gen, state = State.Forced).map {
            case (iter, nextGen) =>
              iter.map {
                case (value, state) =>
                  assert(state == State.Forced)
                  (value, origState)
              } -> (if(nextGen ne gen) Force(nextGen) else self)
          }
        case Pay(gen) if state == State.Unpaid =>
          impl(gen, State.HavePaid)
        case Pay(gen) if state == State.Forced =>
          impl(gen, State.Forced)
        case Pay(_) =>
          Eval.now((LazyList.empty, self))
        case Lazy(genFn) =>
          impl(genFn(), state)
        case Disjunction(left, right) =>
          for {
            (iterLeft, nextGenLeft) <- impl(left, state)
            (iterRight, nextGenRight) <- impl(right, state)
          } yield (
            (iterLeft #::: iterRight) ->
            (if((nextGenLeft eq left) && (nextGenRight eq right)) self else nextGenLeft | nextGenRight)
          )
        case FlatMap(self, fn) =>
          impl(self, state).flatMap {
            case (iter, nextGen) =>
              var finishedIterating = false
              var nextGenAcc = nextGen.flatMap(fn)
              iter
                .flatTraverse {
                  case (value, state) =>
                    impl(fn(value), state).map {
                      case (iter, nextGen) =>
                        nextGenAcc = nextGenAcc | nextGen
                        iter
                    }
                }
                .map { iter =>
                  (iter #::: LazyList.from(iterSentinel { finishedIterating = true }))
                  -> lzy {
                    assert(finishedIterating)
                    nextGenAcc
                  }
                }
          }
      }

    val (iter, nextGen) = impl(this, state = State.Unpaid).value
    val fixedIter = iter
      .iterator
      .collect {
        case (value, State.HavePaid) => Some(value)
        case (value, State.Unpaid) => None
        case (value, State.Forced) => throw AssertionError(s"should never find a top-level forced state")
      }

    (fixedIter, nextGen)
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

  given productGenerator[T](using mirror: deriving.Mirror.ProductOf[T])(using genParts: =>Generator[mirror.MirroredElemTypes]): Generator[T] =
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

  given generatorTupleSingleton[T](using gen: =>Generator[T]): Tuple1[Generator[T]] =
    Tuple1(lzy(gen))
  given generatorTupleCons[T, Rest <: Tuple](using ev: Tuple.InverseMap[Rest, Generator] <:< Tuple)(using tGen: =>Generator[T], rest: Rest): (Generator[T] *: Rest) =
    lzy(tGen) *: rest

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
