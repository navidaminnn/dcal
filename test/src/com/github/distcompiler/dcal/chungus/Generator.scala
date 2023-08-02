package test.com.github.distcompiler.dcal.chungus

import cats.data.Chain
import scala.annotation.targetName
import scala.util.Try

sealed abstract class Generator[T] { self =>
  import Generator.*

  def getSingleton: Option[T] = None
  def isDefinitelyEmpty: Boolean = false
  def apply(budget: Int): LazyList[Either[Result[T], Generator[T]]]

  final def up[U >: T]: Generator[U] =
    self.asInstanceOf[Generator[U]]

  final def filter(fn: T => Boolean): Generator[T] =
    self.flatMap { value =>
      if(fn(value)) {
        free_!(value)
      } else {
        none.up
      }
    }

  final def map[U](fn: T => U): Generator[U] =
    self.flatMap(value => free_!(fn(value)))

  final def flatMap[U](fn: T => Generator[U]): Generator[U] =
    if(self.isDefinitelyEmpty) {
      none.up
    } else {
      self.getSingleton match {
        case Some(value) => fn(value)
        case None => 
          new Generator[U] {
            override def apply(budget: Int): LazyList[Either[Result[U], Generator[U]]] =
              self.apply(budget).flatMap {
                case Left(Result(value, remainder)) =>
                  fn(value)(remainder)
                case Right(nextGen) if nextGen.isDefinitelyEmpty =>
                  Iterator.empty
                case Right(nextGen) =>
                  Iterator.single(Right(nextGen.flatMap(fn)))
              }
          }
      }
    }

  @targetName("or")
  final def |[U](other: Generator[U]): Generator[T | U] =
    if(self.isDefinitelyEmpty) {
      other.up
    } else if(other.isDefinitelyEmpty) {
      self.up
    } else {
      new Generator[T | U] {
        override def apply(budget: Int): LazyList[Either[Result[T | U], Generator[T | U]]] = {
          require(budget >= 0)
          if(budget == 0) {
            Right(this) #:: LazyList.empty
          } else {
            val leftList: LazyList[Either[Result[T | U], Generator[T | U]]] = self.up.apply(budget)
            val rightList: LazyList[Either[Result[T | U], Generator[T | U]]] = other.up.apply(budget)
            if(leftList.nonEmpty && rightList.nonEmpty) {
              leftList #::: rightList
            } else if(leftList.nonEmpty) {
              leftList
            } else {
              rightList
            }
          }
        }
      }
    }

  final def zip[U](other: Generator[U]): Generator[(T, U)] =
    self.flatMap(lhs => other.map((lhs, _)))

  final def flatten[U](using ev: T <:< Generator[U]): Generator[U] =
    self.flatMap(identity)

  final def ++[U](using ev: T <:< Chain[U])(other: Generator[Chain[U]]): Generator[Chain[U]] =
    for {
      lhs <- self
      rhs <- other
    } yield lhs ++ rhs

  object force extends Generator[T] {
    override def getSingleton: Option[T] = self.getSingleton

    override def isDefinitelyEmpty: Boolean = self.isDefinitelyEmpty

    override def apply(budget: Int): LazyList[Either[Result[T], Generator[T]]] = {
      require(budget >= 0)
      self(budget = Int.MaxValue).map {
        case Left(Result(value, _)) => Left(Result(value, budget))
        case Right(nextGen) => Right(nextGen.force)
      }
    }
  }

  final def checkWith(checker: Checker[T]): Unit =
    checker(self)
}

object Generator {
  final case class Result[+T](value: T, remainder: Int)

  final def free_![T](value: T): Generator[T] =
    new Generator[T] {
      override def getSingleton: Option[T] = Some(value)

      override def apply(budget: Int): LazyList[Either[Result[T], Generator[T]]] =
        Left(Result(value, budget)) #:: LazyList.empty
    }

  object none extends Generator[Nothing] {
    override def isDefinitelyEmpty: Boolean = true

    override def apply(budget: Int): LazyList[Either[Result[Nothing], Generator[Nothing]]] =
      LazyList.empty
  }

  val unit: Generator[Unit] = free_!(())

  def one[T](value: T): Generator[T] =
    costOne(free_!(value))

  def lzy[T](generator: =>Generator[T]): Generator[T] =
    new Generator[T] {
      lazy val gen = generator
      override def apply(budget: Int): LazyList[Either[Result[T], Generator[T]]] = gen(budget)
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
    override def apply(budget: Int): LazyList[Either[Result[T], Generator[T]]] = partGensLzy(budget)
  }

  given listOfGenerator[T](using gen: Generator[T]): Generator[List[T]] = listOf(gen)

  def anyOf[T](using gen: =>Generator[T]): Generator[T] = lzy(gen)

  def costOne[T](generator: Generator[T]): Generator[T] =
    if(generator.isDefinitelyEmpty) {
      none.up
    } else {
      new Generator[T] {
        override def apply(budget: Int): LazyList[Either[Result[T], Generator[T]]] = {
          require(budget >= 0)
          if(budget == 0) {
            Right(this) #:: LazyList.empty
          } else {
            generator(budget - 1)
          }
        }
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
