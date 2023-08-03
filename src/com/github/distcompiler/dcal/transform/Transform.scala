package com.github.distcompiler.dcal.transform

import cats.kernel.Monoid
import cats.Comonad
import cats.Id

@FunctionalInterface
trait Transform[A, B] {
  def apply(from: A): B
}

object Transform {
  def apply[A, B](using transform: Transform[A, B]): Transform[A, B] = transform

  private def lzyTrans[A, B](trans: =>Transform[A, B]): Transform[A, B] =
    new Transform[A, B] {
      override def apply(from: A): B = trans(from)
    }

  given transformEmptyTupleMonoid[B](using m: Monoid[B]): Transform[EmptyTuple, B] with {
    override def apply(from: EmptyTuple): B = m.empty
  }

  given transformTupleMonoid[Ah, At <: Tuple, B](using m: Monoid[B])(using transAh: =>Transform[Ah, B], transAt: =>Transform[At, B]): Transform[Ah *: At, B] with {
    override def apply(from: Ah *: At): B =
      m.combine(transAh(from.head), transAt(from.tail))
  }

  given transformProduct[A <: Product, B](using ev: util.NotGiven[A <:< Tuple])(using mirror: deriving.Mirror.ProductOf[A])(using transTuple: =>Transform[mirror.MirroredElemTypes, B]): Transform[A, B] with {
    override def apply(from: A): B =
      transTuple(Tuple.fromProductTyped(from))
  }

  given transformProductM[F[_], A <: Product, B](using ev: util.NotGiven[A <:< Tuple])(using comonad: Comonad[F])(using mirror: deriving.Mirror.ProductOf[A])(using transTuple: =>Transform[mirror.MirroredElemTypes, B]): Transform[F[A], B] with {
    override def apply(from: F[A]): B =
      transTuple(Tuple.fromProductTyped(comonad.extract(from)))
  }

  given tupleOfTransformsOneM[F[_], Hd, B](using transHd: =>Transform[F[Hd], B]): Tuple1[Transform[F[Hd], B]] =
    Tuple1(lzyTrans(transHd))

  given tupleOfTransformsConsM[F[_], Hd, Tl <: NonEmptyTuple, B](using transHd: =>Transform[F[Hd], B], transTl: Tl): (Transform[F[Hd], B] *: Tl) =
    lzyTrans(transHd) *: transTl

  given transformSumM[F[_], A, B](using mirror: deriving.Mirror.SumOf[A])(using com: Comonad[F])(using tupleOfTrans: =>Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform[F[T], B]]): Transform[F[A], B] with {
    override def apply(from: F[A]): B =
      tupleOfTrans.productElement(mirror.ordinal(com.extract(from)))
        .asInstanceOf[Transform[F[A], B]]
        .apply(from)
  }

  given tupleOfTransformsOne[Hd, B](using transHd: =>Transform[Hd, B]): Tuple1[Transform[Hd, B]] =
    Tuple1(lzyTrans(transHd))

  given tupleOfTransformsCons[Hd, Tl <: NonEmptyTuple, B](using transHd: =>Transform[Hd, B], transTl: Tl): (Transform[Hd, B] *: Tl) =
    lzyTrans(transHd) *: transTl

  given transformSum[A, B](using mirror: deriving.Mirror.SumOf[A])(using tupleOfTrans: =>Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform[T, B]]): Transform[A, B] with {
    override def apply(from: A): B =
      tupleOfTrans.productElement(mirror.ordinal(from))
        .asInstanceOf[Transform[A, B]]
        .apply(from)
  }

  given transformList[A, B](using m: Monoid[B])(using aTrans: =>Transform[A, B]): Transform[List[A], B] with {
    override def apply(from: List[A]): B =
      m.combineAll(from.iterator.map(aTrans.apply))
  }
}
