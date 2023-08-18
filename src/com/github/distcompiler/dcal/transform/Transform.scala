package com.github.distcompiler.dcal.transform

import cats.*
import cats.syntax.all.given
import com.github.distcompiler.dcal.util.{SummonFallback, SummonTuple}

opaque type Transform[A, B] = A => B

object Transform {
  opaque type LazyTransform[A, B] = A => B
  object LazyTransform {
    given instance[A, B](using fn: =>Transform[A, B]): LazyTransform[A, B] =
      from => fn(from)
  }

  def fromFn[A, B](fn: A => B): Transform[A, B] = fn

  extension [A, B](self: Transform[A, B]) {
    def asFn: A => B = self

    def apply(from: A): B = self.asFn.apply(from)

    def refine(fn: PartialFunction[A, B]): Transform[A, B] =
      fromFn(fn.applyOrElse(_, self))
  }

  given combineFoldable[F[_], A, B](using Foldable[F], Monoid[B])(using fnA: =>Transform[A, B]): Transform[F[A], B] =
    fromFn(_.foldMap(fnA))

  given mapFunctor[F[_], A, B](using Functor[F])(using fnA: =>Transform[A, B]): Transform[F[A], F[B]] =
    fromFn(_.map(fnA))

  given combineProduct[A <: Product, B](using mirror: deriving.Mirror.ProductOf[A])(using Monoid[B])(using elemFns: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [T] =>> LazyTransform[T, B]]]): Transform[A, B] =
    fromFn { from =>
      (from.productIterator `zip` elemFns.value.productIterator.asInstanceOf[Iterator[LazyTransform[Any, B]]])
        .map {
          case (elem, fn) =>
            fn(elem)
        }
        .foldLeft(Monoid[B].empty)(_ `combine` _)
    }

  given rewriteProduct[A <: Product](using mirror: deriving.Mirror.ProductOf[A])(using elemFns: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform[T, T]]]): Transform[A, A] =
    fromFn { from =>
      mirror.fromTuple {
        Tuple.fromArray {
          (from.productIterator `zip` elemFns.value.productIterator.asInstanceOf[Iterator[Transform[Any, Any]]])
            .map {
              case (elem, fn) =>
                fn(elem)
            }
            .toArray
        }
        .asInstanceOf[mirror.MirroredElemTypes]
      }
    }

  given transformSum[A, B](using tSumK: TSumK[Id, A, B]): Transform[A, B] = tSumK.fn

  final class TSumK[F[_], A, B](val fn: Transform[F[A], B])
  object TSumK {
    given instance[F[_], A, B](using mirror: deriving.Mirror.SumOf[A])(using Comonad[F])(using elemFns: SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [T] =>> SummonFallback[LazyTransform[F[T], B], LazyTransform[T, B]]]]): TSumK[F, A, B] =
      TSumK {
        fromFn { from =>
          elemFns
            .value
            .productElement(mirror.ordinal(from.extract))
            .asInstanceOf[SummonFallback[Transform[F[A], B], Transform[A, B]]]
            .value match {
              case Left(unwrappedFn) => unwrappedFn(from.extract)
              case Right(wrappedFn) => wrappedFn(from)
            }
        }
      }
  }

  given extractComonad[F[_], A, B](using Comonad[F])(using tSumKOrfnA: SummonFallback[TSumK[F, A, B], LazyTransform[A, B]]): Transform[F[A], B] =
    tSumKOrfnA.value match {
      case Left(fnA) =>
        fromFn(from => fnA(from.extract))
      case Right(tSumK) =>
        tSumK.fn
    }

  given coflatMap[F[_], A, B](using CoflatMap[F])(using fnFA: =>Transform[F[A], B]): Transform[F[A], F[B]] =
    fromFn(_.coflatMap(fnFA))
}
