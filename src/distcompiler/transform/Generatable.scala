package distcompiler.transform

import cats.*
import cats.syntax.all.given

import scala.deriving.*
import scala.compiletime.*

import scala.collection.mutable

import izumi.reflect.{Tag, AnyTag}

trait Generatable[T: Tag] { self =>
  protected def mkGeneratorImpl(fn: Generatable.Fn)(using Generatable.Cache): Generator[T]

  final def mkGenerator(fn: Generatable.Fn)(using Generatable.Cache): Generator[T] =
    Generatable.cached(mkGeneratorImpl(fn))

  private given Generatable[T] = this

  final def build(using fn: Generatable.Fn = Generatable.Fn.default): Generatable.Builder[T] =
    Generatable.Builder(fn)

  final def any(using fn: Generatable.Fn = Generatable.Fn.default): Generator[T] =
    build.apply

  final def map[U: Tag](mapFn: T => U): Generatable[U] =
    new Generatable[U] {
      def mkGeneratorImpl(fn: Generatable.Fn)(using Generatable.Cache): Generator[U] =
        fn[U] { () =>
          self.mkGenerator(fn).map(mapFn)
        }
    }
}

object Generatable {
  def apply[T](using gen: Generatable[T]): Generatable[T] = gen

  trait Fn {
    def apply[TT: Tag](defaultRec: () => Generator[TT]): Generator[TT]
  }

  object Fn {
    object default extends Fn {
      def apply[TT](defaultRec: () => Generator[TT])(using Tag[TT]): Generator[TT] =
        defaultRec()
    }
  }

  final class Builder[T: Generatable](defaultFn: Fn) {
    def apply: Generator[T] =
      Generatable[T].mkGenerator(defaultFn)(using mutable.HashMap())

    def replace[I: Tag](gen: Fn ?=> Generator[I]): Builder[T] =
      Builder { 
        new Fn {
          def apply[TT](defaultRec: () => Generator[TT])(using Tag[TT]): Generator[TT] =
            if(Tag[TT] <:< Tag[I]) {
              gen(using this).asInstanceOf[Generator[TT]]
            } else {
              defaultFn(defaultRec)
            }
        }
      }

    def alter[I: Tag](alterFn: Fn ?=> Generator[I] => Generator[I]): Builder[T] =
      Builder {
        new Fn {
          def apply[TT](defaultRec: () => Generator[TT])(using Tag[TT]): Generator[TT] = {
            val result = defaultFn(defaultRec)
            if(Tag[TT] <:< Tag[I]) {
              alterFn(using this)(result.asInstanceOf[Generator[I]]).asInstanceOf[Generator[TT]]
            } else {
              result
            }
          }
        }
      }

    def omit[I: Tag]: Builder[T] =
      replace[I](Generator.empty)
  }

  opaque type Cache = mutable.HashMap[AnyTag, Generator[Any]]

  private[Generatable] def cached[T: Tag](using cache: Cache)(fn: Cache ?=> Generator[T]): Generator[T] =
    cache.get(Tag[T]) match {
      case Some(rec) => rec.asInstanceOf[Generator[T]]
      case None =>
        lazy val gen = fn
        cache.update(Tag[T], Generator.lzy(gen).widen)
        // force evaluation of gen, so we separate construction of Generator and execution of Generator
        val constructedGen = gen
        Generator.lzy(constructedGen)
    }

  inline def derived[T: Tag]: Generatable[T] =
    summonFrom {
      case mirror: Mirror.SumOf[T] => derivedSum[T, mirror.MirroredElemTypes]
      case given Mirror.ProductOf[T] => derivedProduct[T]
    }

  inline def derivedSum[T: Tag, Elems <: Tuple]: Generatable[T] = {
    lazy val elemGens = summonAll[Tuple.Map[Elems, Generatable]]
    new Generatable[T] {
      def mkGeneratorImpl(fn: Fn)(using Cache): Generator[T] =
        fn[T] { () =>
          elemGens
            .productIterator
            .asInstanceOf[Iterator[Generatable[T]]]
            .map(_.mkGenerator(fn))
            .reduceOption(_ | _)
            .getOrElse(Generator.empty)
        }
    }
  }

  inline given derivedProduct[T: Tag](using mirror: Mirror.ProductOf[T]): Generatable[T] = {
    lazy val elemGens = summonAll[Tuple.Map[mirror.MirroredElemTypes, Generatable]]
    new Generatable[T] {
      def mkGeneratorImpl(fn: Fn)(using Cache): Generator[T] =
        fn[T] { () =>
          elemGens
            .productIterator
            .asInstanceOf[Iterator[Generatable[Any]]]
            .map(_.mkGenerator(fn))
            .toList
            .sequence
            .map { elems =>
              mirror.fromProduct(Tuple.fromArray(elems.toArray))
            }
        }
    }
  }

  given derivedAlternative[T, F[_]: Alternative](using gen: =>Generatable[T])(using Tag[F[T]]): Generatable[F[T]] with {
    def mkGeneratorImpl(fn: Fn)(using Cache): Generator[F[T]] =
      fn[F[T]] { () =>
        val theGen = gen.mkGenerator(fn)
        Generator.unfoldLevels() { level =>
          theGen
            .replicateA(level)
            .map(_.foldMapK(_.pure))
        }
      }
  }

  given derivedBoolean: Generatable[Boolean] with {
    def mkGeneratorImpl(fn: Fn)(using Cache): Generator[Boolean] =
      fn[Boolean] { () =>
        Generator.anyFromSeq(List(false, true))
      }
  }

  class PrimitiveError[T: Tag] extends Exception(s"you need to provide an approximate generator for ${Tag[T]}. unrestrained generation of primitives may unpredictably create a huge state space")

  given derivedPrimitive[T: Tag: IsPrimitive]: Generatable[T] with {
    def mkGeneratorImpl(fn: Fn)(using Cache): Generator[T] =
      fn[T] { () =>
        throw PrimitiveError[T]()
      }
  }
}
