package test.com.github.distcompiler.dcal.chungus

import scala.annotation.targetName
import cats.Eval
import cats.data.Chain

sealed abstract class Generator[+T] { self =>
  import Generator.*

  def apply(budget: Int): Chain[Result[T]]

  final def map[U](fn: T => U): Generator[U] =
    new Generator[U] {
      override def apply(budget: Int): Chain[Result[U]] =
        self.apply(budget).map {
          case Result(value, remainder) =>
            Result(fn(value), remainder)
        }
    }

  final def flatMap[U](fn: T => Generator[U]): Generator[U] =
    new Generator[U] {
      override def apply(budget: Int): Chain[Result[U]] =
        self.apply(budget).flatMap {
          case Result(value, remainder) =>
            fn(value).apply(remainder)
        }
    }

  @targetName("or")
  final def |[U](other: Generator[U]): Generator[T | U] =
    new Generator[T | U] {
      override def apply(budget: Int): Chain[Result[T | U]] = {
        require(budget >= 0)
        if(budget == 0) {
          Chain.nil
        } else {
          self.apply(budget) ++ other.apply(budget)
        }
      }
    }

  final def forall(budget: Int)(fn: T => Any): Unit =
    self.apply(budget).iterator.foreach {
      case Result(value, _) => fn(value)
    }
}

object Generator {
  final case class Result[+T](value: T, remainder: Int)

  object none extends Generator[Nothing] {
    override def apply(budget: Int): Chain[Result[Nothing]] =
      Chain.nil
  }

  object unit extends Generator[Unit] {
    override def apply(budget: Int): Chain[Result[Unit]] =
      Chain.one(Result((), budget))
  }

  def one[T](value: T): Generator[T] =
    new Generator[T] {
      override def apply(budget: Int): Chain[Result[T]] = {
        require(budget >= 0)
        if(budget == 0) {
          Chain.nil
        } else {
          Chain.one(Result(value, budget - 1))
        }
      }
    }

  given tupleGeneratorEmpty: Generator[EmptyTuple] = unit.map(_ => EmptyTuple)
  given tupleGeneratorCons[T, Rest <: Tuple](using tGen: Generator[T], restGen: Generator[Rest]): Generator[T *: Rest] =
    for {
      t <- tGen
      rest <- restGen
    } yield t *: rest

  given productGenerator[T](using mirror: deriving.Mirror.ProductOf[T])(using genParts: Generator[mirror.MirroredElemTypes]): Generator[T] =
    costOne(genParts.map(mirror.fromTuple))

  given generatorTupleSingleton[T](using gen: Generator[T]): Tuple1[Generator[T]] =
    Tuple1(gen)
  given generatorTupleCons[T, Rest <: Tuple](using ev: Tuple.InverseMap[Rest, Generator] <:< Tuple)(using tGen: Generator[T], rest: Rest): (Generator[T] *: Rest) =
    tGen *: rest

  given sumGenerator[T](using mirror: deriving.Mirror.SumOf[T])(using partGens: Tuple.Map[mirror.MirroredElemTypes, Generator]): Generator[T] =
    partGens
      .productIterator
      .foldLeft(none: Generator[T]) { (acc, gen) =>
        acc | gen.asInstanceOf[Generator[T]]
      }

  def anyOf[T](using gen: Generator[T]): Generator[T] = gen

  def costOne[T](generator: Generator[T]): Generator[T] =
    new Generator[T] {
      override def apply(budget: Int): Chain[Result[T]] = {
        require(budget >= 0)
        generator.apply(math.max(0, budget - 1))
      }
    }

  def listOf[T](generator: Generator[T]): Generator[List[T]] =
    one(Nil)
    | costOne {
        for {
          head <- generator
          tail <- listOf(generator)
        } yield head :: tail
      }
}
