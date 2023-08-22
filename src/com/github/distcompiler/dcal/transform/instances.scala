package com.github.distcompiler.dcal.transform

import cats.*
import cats.syntax.all.given

import com.github.distcompiler.dcal.util.{SummonTuple, SummonFallback}
import scala.runtime.MatchCase

object instances {
  object all extends Priority1

  trait Priority1 extends Priority2 {
    given combineProduct[A <: Product, B](using mirror: deriving.Mirror.ProductOf[A])(using Monoid[B])(using elemFns: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform.Lazy[T, B]]]): Transform.Generic[A, B] =
      Transform.Generic.fromFunction { from =>
        (from.productIterator `zip` elemFns.value.productIterator.asInstanceOf[Iterator[Transform.Lazy[Any, B]]])
          .map {
            case (elem, fn) =>
              fn.asTransform(elem)
          }
          .foldLeft(Monoid[B].empty)(_ `combine` _)
      }

    given rewriteProduct[A <: Product](using mirror: deriving.Mirror.ProductOf[A])(using elemFns: SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform.Lazy[T, T]]]): Transform.Generic[A, A] =
      Transform.Generic.fromFunction { from =>
        mirror.fromTuple {
          Tuple.fromArray {
            (from.productIterator `zip` elemFns.value.productIterator.asInstanceOf[Iterator[Transform.Lazy[Any, Any]]])
              .map {
                case (elem, fn) =>
                  fn.asTransform(elem)
              }
              .toArray
          }
          .asInstanceOf[mirror.MirroredElemTypes]
        }
      }

    given rewriteProductK[F[_], A <: Product](using mirror: deriving.Mirror.ProductOf[A])(using Applicative[F])(using elemFns: SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform.Lazy[T, F[T]]]]): Transform.Generic[A, F[A]] =
      Transform.Generic.fromFunction { from =>
        (from.productIterator `zip` elemFns.value.productIterator.asInstanceOf[Iterator[Transform.Lazy[Any, F[Any]]]])
          .map {
            case (elem, fn) =>
              fn.asTransform(elem)
          }
          .toList
          .sequence
          .map { elems =>
            mirror.fromTuple(Tuple.fromArray(elems.toArray).asInstanceOf[mirror.MirroredElemTypes])
          }
      }

    private def sumImpl[A, B, Tpl <: Tuple](elemFns: SummonTuple[Tpl], ordinal: Int): Transform[A, B] =
      elemFns
        .value
        .productElement(ordinal)
        .asInstanceOf[Transform.Lazy[A, B]]
        .asTransform

    given combineSum[A, B](using mirror: deriving.Mirror.SumOf[A])(using elemFns: SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform.Lazy[T, B]]]): Transform.Generic[A, B] =
      Transform.Generic.fromFunction { from =>
        sumImpl(elemFns, mirror.ordinal(from))(from)
      }

    given rewriteSum[A](using mirror: deriving.Mirror.SumOf[A])(using elemFns: SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform.Lazy[T, T]]]): Transform.Generic[A, A] =
      Transform.Generic.fromFunction { from =>
        sumImpl(elemFns, mirror.ordinal(from))(from)
      }

    given rewriteSumK[F[_], A](using mirror: deriving.Mirror.SumOf[A])(using elemFns: SummonTuple[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform.Lazy[T, F[T]]]]): Transform.Generic[A, F[A]] =
      Transform.Generic.fromFunction { from =>
        sumImpl(elemFns, mirror.ordinal(from))(from)
      }
  }

  trait Priority2 extends Priority3 {
    given combineFoldable[F[_], A, B](using Foldable[F], Monoid[B])(using fnA: =>Transform[A, B]): Transform.Generic[F[A], B] =
      Transform.Generic.fromFunction(_.foldMap(fnA.asFunction))

    given mapFunctor[F[_], A, B](using Functor[F])(using fnA: =>Transform[A, B]): Transform.Generic[F[A], F[B]] =
      Transform.Generic.fromFunction(_.map(fnA.asFunction))
  }

  trait Priority3 {
    given flatMap[F[_], A, B](using FlatMap[F])(using fnA: =>Transform[A, F[B]]): Transform.Generic[F[A], F[B]] =
      Transform.Generic.fromFunction(_.flatMap(fnA.asFunction))

    given extractComonad[F[_], A, B](using Comonad[F])(using fnA: =>Transform[A, B]): Transform.Generic[F[A], B] =
      Transform.Generic.fromFunction(from => fnA(from.extract))

    given coflatMap[F[_], A, B](using CoflatMap[F])(using fnFA: =>Transform[F[A], B]): Transform.Generic[F[A], F[B]] =
      Transform.Generic.fromFunction(_.coflatMap(fnFA.asFunction))
  }
}
