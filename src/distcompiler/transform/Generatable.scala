package distcompiler.transform

import cats.*
import cats.syntax.all.given

import scala.deriving.*
import scala.compiletime.*

import scala.collection.mutable

import izumi.reflect.{Tag, AnyTag}

trait Generatable[T: Tag] { self =>
  protected def mkGeneratorImpl(fn: Generatable.Fn)(using Generatable.Cache): Eval[Generator[T]]

  final def mkGenerator(fn: Generatable.Fn)(using Generatable.Cache): Eval[Generator[T]] =
    Generatable.cached(mkGeneratorImpl(fn))

  private given Generatable[T] = this

  final def build(using fn: Generatable.Fn = Generatable.Fn.default): Generatable.Builder[T] =
    Generatable.Builder(fn)

  final def any(using fn: Generatable.Fn = Generatable.Fn.default)(using cache: Generatable.Cache = Generatable.Cache.freshEmpty): Generator[T] =
    build.apply

  final def map[U: Tag](mapFn: T => U): Generatable[U] =
    new Generatable[U] {
      def mkGeneratorImpl(fn: Generatable.Fn)(using Generatable.Cache): Eval[Generator[U]] =
        fn[U](self.mkGenerator(fn).map(_.map(mapFn)))
    }
}

object Generatable {
  def apply[T](using gen: Generatable[T]): Generatable[T] = gen

  trait Fn {
    def apply[TT: Tag](defaultRec: Eval[Generator[TT]])(using Cache): Eval[Generator[TT]]
  }

  object Fn {
    object default extends Fn {
      def apply[TT: Tag](defaultRec: Eval[Generator[TT]])(using Cache): Eval[Generator[TT]] =
        defaultRec
    }
  }

  final class Builder[T: Generatable](defaultFn: Fn) {
    def apply(using cache: Cache = Cache.freshEmpty): Generator[T] =
      Generatable[T].mkGenerator(defaultFn).value

    def replace[I: Tag](gen: Fn ?=> Cache ?=> Generator[I]): Builder[T] =
      replaceEval[I](Eval.now(gen))

    def replaceEval[I: Tag](gen: Fn ?=> Cache ?=> Eval[Generator[I]]): Builder[T] =
      Builder { 
        new Fn {
          def apply[TT](defaultRec: Eval[Generator[TT]])(using Tag[TT], Cache): Eval[Generator[TT]] =
            Eval.defer {
              if(Tag[TT] <:< Tag[I]) {
                gen(using this).asInstanceOf[Eval[Generator[TT]]]
              } else {
                defaultFn(defaultRec)
              }
            }
        }
      }

    def alter[I: Tag](alterFn: Fn ?=> Cache  ?=> Generator[I] => Generator[I]): Builder[T] =
      Builder {
        new Fn {
          def apply[TT](defaultRec: Eval[Generator[TT]])(using Tag[TT], Cache): Eval[Generator[TT]] = {
            val result = defaultFn(defaultRec)
            if(Tag[TT] <:< Tag[I]) {
              result.map { result =>
                alterFn(using this)(result.asInstanceOf[Generator[I]]).asInstanceOf[Generator[TT]]
              }
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

  object Cache {
    def freshEmpty: Cache = mutable.HashMap()
  }

  private[Generatable] def cached[T: Tag](using cache: Cache)(fn: Cache ?=> Eval[Generator[T]]): Eval[Generator[T]] =
    cache.get(Tag[T]) match {
      case Some(rec) => Eval.now(rec.asInstanceOf[Generator[T]])
      case None =>
        val gen: Eval[Generator[T]] = fn.memoize
        cache.update(Tag[T], Generator.lzy(gen.value).widen)
        // force evaluation of gen, so we separate construction of Generator and execution of Generator
        gen.map(Generator.lzy)
    }

  inline def derived[T: Tag]: Generatable[T] =
    summonFrom {
      case mirror: Mirror.SumOf[T] => derivedSum[T, mirror.MirroredElemTypes]
      case given Mirror.ProductOf[T] => derivedProduct[T]
    }

  inline def derivedSum[T: Tag, Elems <: Tuple]: Generatable[T] = {
    lazy val elemGens = summonAll[Tuple.Map[Elems, Generatable]]
    new Generatable[T] {
      def mkGeneratorImpl(fn: Fn)(using Cache): Eval[Generator[T]] =
        Eval.defer(fn[T](Eval.defer {
          elemGens
            .productIterator
            .asInstanceOf[Iterator[Generatable[T]]]
            .map(_.mkGenerator(fn))
            .reduceOption((l, r) => (l, r).mapN(_ | _))
            .getOrElse(Eval.now(Generator.empty))
        }))
    }
  }

  inline given derivedProduct[T: Tag](using mirror: Mirror.ProductOf[T]): Generatable[T] = {
    lazy val elemGens = summonAll[Tuple.Map[mirror.MirroredElemTypes, Generatable]]
    new Generatable[T] {
      def mkGeneratorImpl(fn: Fn)(using Cache): Eval[Generator[T]] =
        Eval.defer(fn[T](Eval.defer {
          elemGens
            .productIterator
            .asInstanceOf[Iterator[Generatable[Any]]]
            .map(_.mkGenerator(fn))
            .toList
            .sequence
            .map(_.sequence)
            .map(_.map { elems =>
              mirror.fromProduct(Tuple.fromArray(elems.toArray))
            })
        }))
    }
  }

  given derivedAlternative[T, F[_]: Alternative](using gen: =>Generatable[T])(using Tag[F[T]]): Generatable[F[T]] with {
    def mkGeneratorImpl(fn: Fn)(using Cache): Eval[Generator[F[T]]] =
      Eval.defer(fn[F[T]](Eval.defer {
        gen.mkGenerator(fn).map { theGen =>
          Generator.unfoldLevels() { level =>
            theGen
              .replicateA(level)
              .map(_.foldMapK(_.pure))
          }
        }
      }))
  }

  given derivedBoolean: Generatable[Boolean] with {
    def mkGeneratorImpl(fn: Fn)(using Cache): Eval[Generator[Boolean]] =
      Eval.defer(fn[Boolean] {
        Eval.now(Generator.anyFromSeq(List(false, true)))
      })
  }

  class PrimitiveError[T: Tag] extends Exception(s"you need to provide an approximate generator for ${Tag[T]}. unrestrained generation of primitives may unpredictably create a huge state space")

  given derivedPrimitive[T: Tag: IsPrimitive]: Generatable[T] with {
    def mkGeneratorImpl(fn: Fn)(using Cache): Eval[Generator[T]] =
      Eval.defer(fn[T](Eval.later(throw PrimitiveError[T]())))
  }
}
