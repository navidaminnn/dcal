package test.com.github.distcompiler.dcal.chungus

import cats.data.Chain
import scala.annotation.targetName
import scala.util.Try

sealed abstract class Generator[T] { self =>
  import Generator.*

  def apply(budget: Int): LazyList[Result[T]]

  final def up[U >: T]: Generator[U] =
    self.map(value => value)

  final def filter(fn: T => Boolean): Generator[T] =
    new Generator[T] {
      override def apply(budget: Int): LazyList[Result[T]] =
        self(budget).filter {
          case Result(value, _) => fn(value)
        }
    }

  final def map[U](fn: T => U): Generator[U] =
    new Generator[U] {
      override def apply(budget: Int): LazyList[Result[U]] =
        self.apply(budget).map {
          case Result(value, remainder) =>
            Result(fn(value), remainder)
        }
    }

  final def flatMap[U](fn: T => Generator[U]): Generator[U] =
    new Generator[U] {
      override def apply(budget: Int): LazyList[Result[U]] =
        self.apply(budget).flatMap {
          case Result(value, remainder) =>
            fn(value).apply(remainder)
        }
    }

  @targetName("or")
  final def |[U](other: Generator[U]): Generator[T | U] =
    new Generator[T | U] {
      override def apply(budget: Int): LazyList[Result[T | U]] = {
        require(budget >= 0)
        if(budget == 0) {
          LazyList.empty
        } else {
          self.apply(budget) ++ other.apply(budget)
        }
      }
    }

  final def zip[U](other: Generator[U]): Generator[(T, U)] =
    self.flatMap(lhs => other.map(lhs -> _))

  final def flatten[U](using ev: T <:< Generator[U]): Generator[U] =
    self.flatMap(identity)

  final def ++[U](using ev: T <:< Chain[U])(other: Generator[Chain[U]]): Generator[Chain[U]] =
    for {
      lhs <- self
      rhs <- other
    } yield lhs ++ rhs

  object force extends Generator[T] {
    override def apply(budget: Int): LazyList[Result[T]] = {
      require(budget >= 0)
      self(budget = Int.MaxValue).map {
        case Result(value, _) => Result(value, budget)
      }
    }
  }

  final def checkWith(checker: Checker[T]): Unit =
    checker(self)
}

object Generator {
  final case class Result[+T](value: T, remainder: Int)

  object none extends Generator[Nothing] {
    override def apply(budget: Int): LazyList[Result[Nothing]] =
      LazyList.empty
  }

  object unit extends Generator[Unit] {
    override def apply(budget: Int): LazyList[Result[Unit]] =
      LazyList(Result((), budget))
  }

  def one[T](value: T): Generator[T] =
    new Generator[T] {
      override def apply(budget: Int): LazyList[Result[T]] = {
        require(budget >= 0)
        if(budget == 0) {
          LazyList.empty
        } else {
          LazyList(Result(value, budget - 1))
        }
      }
    }

  def lzy[T](generator: =>Generator[T]): Generator[T] =
    new Generator[T] {
      lazy val gen = generator
      override def apply(budget: Int): LazyList[Result[T]] = gen(budget)
    }

  given tupleGeneratorEmpty: Generator[EmptyTuple] = unit.map(_ => EmptyTuple)
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
          .map(value => unit.map(_ => value))
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

  given sumGenerator[T](using mirror: deriving.Mirror.SumOf[T])(using partGens: =>Tuple.Map[mirror.MirroredElemTypes, Generator]): Generator[T] with {
    lazy val partGensLzy =
      partGens
        .productIterator
        .foldLeft(none.up: Generator[T]) { (acc, gen) =>
          acc | gen.asInstanceOf[Generator[T]]
        }
    override def apply(budget: Int): LazyList[Result[T]] = partGensLzy(budget)
  }

  given listOfGenerator[T](using gen: Generator[T]): Generator[List[T]] = listOf(gen)

  def anyOf[T](using gen: =>Generator[T]): Generator[T] = lzy(gen)

  def costOne[T](generator: Generator[T]): Generator[T] =
    new Generator[T] {
      override def apply(budget: Int): LazyList[Result[T]] = {
        require(budget >= 0)
        generator.apply(math.max(0, budget - 1))
      }
    }

  def chooseAny[T](iterable: Iterable[T]): Generator[T] =
    iterable.iterator
      .map(one)
      .reduceOption(_ | _)
      .getOrElse(none.up)

  def listOf[T](generator: Generator[T], limit: Int = Int.MaxValue): Generator[List[T]] = {
    require(limit >= 0)
    if(limit == 0) {
      none.up
    } else {
      one(Nil)
      | costOne {
          for {
            head <- generator
            tail <- listOf(generator, limit = limit - 1)
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
