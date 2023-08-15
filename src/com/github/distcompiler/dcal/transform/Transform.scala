package com.github.distcompiler.dcal.transform

import cats.*
import cats.syntax.all.given

trait Transform[A, B] { self =>
  def apply(from: A): B

  final def refine(fn: PartialFunction[A, B]): Transform[A, B] =
    new Transform[A, B] {
      override def apply(from: A): B =
        fn.applyOrElse(from, self.apply)
    }
}

object Transform {
  def apply[A, B](using transform: Transform[A, B]): Transform[A, B] = transform

  def lzyTrans[A, B](trans: =>Transform[A, B]): Transform[A, B] =
    new Transform[A, B] {
      override def apply(from: A): B = trans(from)
    }

  given transformList[A, B](using m: Monoid[B])(using aTrans: =>Transform[A, B]): Transform[List[A], B] with {
    override def apply(from: List[A]): B =
      m.combineAll(from.iterator.map(aTrans.apply))
  }

  given transformProduct[A <: Product, B](using mirror: deriving.Mirror.ProductOf[A])(using Monoid[B])(using elemTrans: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform[T, B]]]): Transform[A, B] with {
    override def apply(from: A): B =
      (from.productIterator `zip` elemTrans.value.productIterator.asInstanceOf[Iterator[Transform[Any, B]]])
        .map {
          case (elem, trans) =>
            trans(elem)
        }
        .foldLeft(Monoid[B].empty)(_ `combine` _)
  }

  given transformProductK[F[_], A <: Product, B](using Comonad[F])(using mirror: deriving.Mirror.ProductOf[A])(using withoutF: Transform[mirror.MirroredElemTypes, B]): Transform[F[A], B] with {
    override def apply(from: F[A]): B =
      withoutF(Tuple.fromProductTyped(from.extract))
  }

  given transformSum[A, B](using mirror: deriving.Mirror.SumOf[A])(using transElems: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform[T, B]]]): Transform[A, B] with {
    override def apply(from: A): B =
      transElems.value.productElement(mirror.ordinal(from))
        .asInstanceOf[Transform[A, B]]
        .apply(from)
  }

  final class TransformSumK[F[_], A, B](val transform: Transform[F[A], B])

  given transformSumK[F[_], A, B](using mirror: deriving.Mirror.SumOf[A])(using Comonad[F])(using transElems: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, [T] =>> SummonFallback.SF[Transform[F[T], B], Transform[T, B]]]]): TransformSumK[F, A, B] =
    TransformSumK {
      new Transform[F[A], B] {
        override def apply(from: F[A]): B =
          transElems
            .value
            .productElement(mirror.ordinal(from.extract))
            .asInstanceOf[SummonFallback.SF[Transform[F[A], B], Transform[A, B]]]
            .value match {
              case Left(unwrappedFn) => unwrappedFn(from.extract)
              case Right(wrappedFn) => wrappedFn(from)
            }
      }
    }

  given transformMaybeSumK[F[_], A, B](using Comonad[F])(using eitherOr: SummonFallback.SF[TransformSumK[F, A, B], Transform[A, B]]): Transform[F[A], B] =
    eitherOr.value match {
      case Left(transInner) =>
        new Transform[F[A], B] {
          override def apply(from: F[A]): B =
            transInner(from.extract)
        }
      case Right(transSumK) => transSumK.transform
    }
}
