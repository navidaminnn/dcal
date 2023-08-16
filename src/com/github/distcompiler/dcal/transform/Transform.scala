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

  given combineFoldable[F[_], A, B](using Foldable[F], Monoid[B])(using aTrans: Transform[A, B]): Transform[F[A], B] with {
    override def apply(from: F[A]): B =
      from.foldMap(aTrans.apply)
  }

  given mapFunctor[F[_], A, B](using Functor[F])(using aTrans: Transform[A, B]): Transform[F[A], F[B]] with {
    override def apply(from: F[A]): F[B] =
      from.map(aTrans.apply)
  }

  given combineProduct[A <: Product, B](using mirror: deriving.Mirror.ProductOf[A])(using Monoid[B])(using elemTrans: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform[T, B]]]): Transform[A, B] with {
    override def apply(from: A): B =
      (from.productIterator `zip` elemTrans.value.productIterator.asInstanceOf[Iterator[Transform[Any, B]]])
        .map {
          case (elem, trans) =>
            trans(elem)
        }
        .foldLeft(Monoid[B].empty)(_ `combine` _)
  }

  given rewriteProduct[A <: Product](using mirror: deriving.Mirror.ProductOf[A])(using elemTrans: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, [T] =>> Transform[T, T]]]): Transform[A, A] with {
    override def apply(from: A): A =
      mirror.fromTuple {
        Tuple.fromArray {
          (from.productIterator `zip` elemTrans.value.productIterator.asInstanceOf[Iterator[Transform[Any, Any]]])
            .map {
              case (elem, trans) =>
                trans(elem)
            }
            .toArray
        }
        .asInstanceOf[mirror.MirroredElemTypes]
      }
  }

  given transformSum[A, B](using tSumK: TSumK[Id, A, B]): Transform[A, B] with {
    override def apply(from: A): B =
      tSumK.transform(from)
  }

  final class TSumK[F[_], A, B](val transform: Transform[F[A], B])
  object TSumK {
    given instance[F[_], A, B](using mirror: deriving.Mirror.SumOf[A])(using Comonad[F])(using transElems: =>SummonTuple.ST[Tuple.Map[mirror.MirroredElemTypes, [T] =>> SummonFallback.SF[Transform[F[T], B], Transform[T, B]]]]): TSumK[F, A, B] =
      TSumK {
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
  }

  given extractComonad[F[_], A, B](using Comonad[F])(using eitherOr: SummonFallback.SF[TSumK[F, A, B], Transform[A, B]]): Transform[F[A], B] =
    eitherOr.value match {
      case Left(transInner) =>
        new Transform[F[A], B] {
          override def apply(from: F[A]): B =
            transInner(from.extract)
        }
      case Right(transSumK) => transSumK.transform
    }

  given coflatMap[F[_], A, B](using CoflatMap[F])(using transFn: Transform[F[A], B]): Transform[F[A], F[B]] with {
    override def apply(from: F[A]): F[B] =
      from.coflatMap(transFn.apply)
  }
}
