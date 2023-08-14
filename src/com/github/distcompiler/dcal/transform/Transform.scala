package com.github.distcompiler.dcal.transform

import cats.*
import cats.syntax.all.given

@FunctionalInterface
trait Transform[A, B] {
  def apply(from: A): B
}

object Transform {
  def apply[A, B](using transform: Transform[A, B]): Transform[A, B] = transform

  def lzyTrans[A, B](trans: =>Transform[A, B]): Transform[A, B] =
    new Transform[A, B] {
      override def apply(from: A): B = trans(from)
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

  given transformSumK[F[_], A, B](using mirror: deriving.Mirror.SumOf[A])(using Comonad[F])(using transElems: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, [T] =>> SummonFallback.SF[Transform[F[T], B], Transform[T, B]]]]): Transform[F[A], B] with {
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

  given transformList[A, B](using m: Monoid[B])(using aTrans: =>Transform[A, B]): Transform[List[A], B] with {
    override def apply(from: List[A]): B =
      m.combineAll(from.iterator.map(aTrans.apply))
  }
}
