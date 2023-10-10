package distcompiler.transform

import distcompiler.util.{SummonTuple, SummonFallback}

import cats.*
import scala.deriving.Mirror

trait Poly {
  trait Case[-T, +U] {
    def apply(t: T): U
  }

  trait GeneralCase[-T, +U] extends Case[T, U]
  trait SpecialCase[-T, +U] extends Case[T, U]

  final def apply[T, U](arg: T)(using cs: Case[T, U]): U = cs(arg)

  final def applyGeneral[T, U](arg: T)(using cs: GeneralCase[T, U]): U = cs(arg)
}

object Poly {
  transparent trait Identity { self: Poly =>
    given identityCase[T]: GeneralCase[T, T] with {
      def apply(t: T): T = t
    }
  }

  transparent trait IdentityA { self: Poly =>
    given identityACase[F[_]: Applicative, T]: GeneralCase[T, F[T]] with {
      def apply(t: T): F[T] = Applicative[F].pure(t)
    }
  }

  transparent trait FunctorTransform { self: Poly =>
    given functorTransformCase[F[_]: Functor, T1, T2](using subcase: =>Case[T1, T2]): GeneralCase[F[T1], F[T2]] with {
      def apply(t: F[T1]): F[T2] =
        Functor[F].map(t)(subcase.apply)
    }
  }

  transparent trait TraverseTransformA { self: Poly =>
    given traverseTransformACase[F[_]: Traverse, G[_]: Applicative, T1, T2](using subcase: =>Case[T1, G[T2]]): GeneralCase[F[T1], G[F[T2]]] with {
      def apply(t: F[T1]): G[F[T2]] =
        Traverse[F].traverse(t)(subcase.apply)
    }
  }

  transparent trait GenericTransform { self: Poly =>
    given sumTransformCase[T1, T2](
      using mirror1: Mirror.SumOf[T1],
      mirror2: Mirror.SumOf[T2],
      ev: Tuple.Size[mirror1.MirroredElemTypes] =:= Tuple.Size[mirror2.MirroredElemTypes],
      subCases: =>SummonTuple[Tuple.Map[Tuple.Zip[mirror1.MirroredElemTypes, mirror2.MirroredElemTypes], [P] =>> P match { case (t1, t2) => Case[t1, t2] }]]
    ): GeneralCase[T1, T2] with {
      def apply(t: T1): T2 =
        subCases.value.productElement(mirror1.ordinal(t)).asInstanceOf[Case[T1, T2]].apply(t)
    }

    given singletonTransformCase[T1 <: Singleton, T2 <: Singleton](using mirror: Mirror.ProductOf[T2]): GeneralCase[T1, T2] with {
      def apply(t: T1): T2 = mirror.fromProductTyped(EmptyTuple)
    }

    given productTransformCase[T1 <: Product, T2 <: Product](
      using mirror1: Mirror.ProductOf[T1],
      mirror2: Mirror.ProductOf[T2],
      ev: Tuple.Size[mirror1.MirroredElemTypes] =:= Tuple.Size[mirror2.MirroredElemTypes],
      subCases: =>SummonTuple[Tuple.Map[Tuple.Zip[mirror1.MirroredElemTypes, mirror2.MirroredElemTypes], [P] =>> P match { case (t1, t2) => Case[t1, t2] }]]
    ): GeneralCase[T1, T2] with {
      def apply(t: T1): T2 =
        mirror2.fromProduct {
          Tuple.fromArray {
            (subCases.value.productIterator.asInstanceOf[Iterator[Case[Any, Any]]] `zip` t.productIterator)
              .map(_.apply(_))
              .toArray
          }
        }
    }
  }

  transparent trait GenericTransformA { self: Poly =>
    given sumTransformACase[F[_]: Applicative, T1, T2](
      using mirror1: Mirror.SumOf[T1],
      mirror2: Mirror.SumOf[T2],
      ev: Tuple.Size[mirror1.MirroredElemTypes] =:= Tuple.Size[mirror2.MirroredElemTypes],
      subCases: =>SummonTuple[Tuple.Map[Tuple.Zip[mirror1.MirroredElemTypes, mirror2.MirroredElemTypes], [P] =>> P match { case (t1, t2) => Case[t1, F[t2]] }]]
    ): GeneralCase[T1, F[T2]] with {
      def apply(t: T1): F[T2] =
        subCases.value.productElement(mirror1.ordinal(t)).asInstanceOf[Case[T1, F[T2]]].apply(t)
    }

    given singletonTransformACase[F[_]: Applicative, T1 <: Singleton, T2 <: Singleton](using mirror: Mirror.ProductOf[T2]): GeneralCase[T1, F[T2]] with {
      def apply(t: T1): F[T2] = Applicative[F].pure(mirror.fromProductTyped(EmptyTuple))
    }

    given productTransformACase[F[_]: Applicative, T1 <: Product, T2 <: Product](
      using mirror1: Mirror.ProductOf[T1],
      mirror2: Mirror.ProductOf[T2],
      ev: Tuple.Size[mirror1.MirroredElemTypes] =:= Tuple.Size[mirror2.MirroredElemTypes],
      subCases: =>SummonTuple[Tuple.Map[Tuple.Zip[mirror1.MirroredElemTypes, mirror2.MirroredElemTypes], [P] =>> P match { case (t1, t2) => Case[t1, F[t2]] }]]
    ): GeneralCase[T1, F[T2]] with {
      def apply(t: T1): F[T2] =
        Applicative[F].map {
          Traverse[List].sequence {
            (subCases.value.productIterator.asInstanceOf[Iterator[Case[Any, F[Any]]]] `zip` t.productIterator)
              .map(_.apply(_))
              .toList
          }
        } { elems =>
          mirror2.fromProduct(Tuple.fromArray(elems.toArray))
        }
    }
  }
}
