package distcompiler.transform

import scala.compiletime.*
import scala.deriving.*

import cats.*
import cats.syntax.all.given

import izumi.reflect.Tag

trait Transformable[T] {
  def doCombine[U: Monoid](t: T, fn: [TT] => (TT => U) => Tag[TT] ?=> TT => U): U

  def doRewrite(t: T, fn: [TT] => (TT => TT) => Tag[TT] ?=> TT => TT): T

  def doRewriteA[F[_]: Applicative](t: T, fn: [TT] => (TT => F[TT]) => Tag[TT] ?=> TT => F[TT]): F[T]

  private given Transformable[T] = this

  def combining[U: Monoid]: Transformable.CombineBuilder[T, U] =
    Transformable.CombineBuilder[T, U]([TT] => (defaultRec: TT => U) => defaultRec)

  def rewriting: Transformable.RewriteBuilder[T] =
    Transformable.RewriteBuilder[T]([TT] => (defaultRec: TT => TT) => defaultRec)

  def rewritingA[F[_]: Applicative]: Transformable.RewriteABuilder[T, F] =
    Transformable.RewriteABuilder[T, F]([TT] => (defaultRec: TT => F[TT]) => defaultRec)
}

object Transformable {
  def apply[T](using trans: Transformable[T]): Transformable[T] = trans

  inline def derived[T: Tag]: Transformable[T] =
    summonFrom {
      case given Mirror.ProductOf[T] => derivedProduct[T]
      case mirror: Mirror.SumOf[T] => derivedSum[T, mirror.MirroredElemTypes](mirror.ordinal)
    }

  inline def derivedSum[T: Tag, Elems <: Tuple](inline ordinal: T => Int): Transformable[T] = {
    lazy val elemTransforms = summonAll[Tuple.Map[Elems, Transformable]]
    new Transformable[T] {
      def theTransformable(t: T): Transformable[T] =
        elemTransforms
          .productElement(ordinal(t))
          .asInstanceOf[Transformable[T]]

      def doCombine[U](t: T, fn: [TT] => (TT => U) => Tag[TT] ?=> TT => U)(using Monoid[U]): U =
        fn[T](t => theTransformable(t).doCombine(t, fn))(t)

      def doRewrite(t: T, fn: [TT] => (TT => TT) => Tag[TT] ?=> TT => TT): T =
        fn[T](t => theTransformable(t).doRewrite(t, fn))(t)

      def doRewriteA[F[_]](t: T, fn: [TT] => (TT => F[TT]) => Tag[TT] ?=> TT => F[TT])(using Applicative[F]): F[T] =
        fn[T](t => theTransformable(t).doRewriteA(t, fn))(t)
    }
  }

  inline given derivedProduct[T: Tag](using mirror: Mirror.ProductOf[T]): Transformable[T] = {
    lazy val elemTransforms = summonAll[Tuple.Map[mirror.MirroredElemTypes, Transformable]]
    new Transformable[T] {
      def doCombine[U](t: T, fn: [TT] => (TT => U) => Tag[TT] ?=> TT => U)(using Monoid[U]): U =
        fn[T] { t =>
          (elemTransforms.productIterator `zip` t.asInstanceOf[T & Product].productIterator)
            .asInstanceOf[Iterator[(Transformable[Any], Any)]]
            .map(_.doCombine[U](_, fn))
            .foldLeft(Monoid[U].empty)(_ `combine` _)
        }(t)

      def doRewrite(t: T, fn: [TT] => (TT => TT) => Tag[TT] ?=> TT => TT): T =
        fn[T] { t =>
          mirror.fromProduct(Tuple.fromArray {
            (elemTransforms.productIterator `zip` t.asInstanceOf[T & Product].productIterator)
              .asInstanceOf[Iterator[(Transformable[Any], Any)]]
              .map(_.doRewrite(_, fn))
              .toArray
          })
        }(t)

      def doRewriteA[F[_]](t: T, fn: [TT] => (TT => F[TT]) => Tag[TT] ?=> TT => F[TT])(using Applicative[F]): F[T] =
        fn[T] { t =>
          (elemTransforms.productIterator `zip` t.asInstanceOf[T & Product].productIterator)
              .asInstanceOf[Iterator[(Transformable[Any], Any)]]
              .map(_.doRewriteA[F](_, fn))
              .toList
              .sequence
              .map { elems =>
                mirror.fromProduct(Tuple.fromArray(elems.toArray))
              }
        }(t)
    }
  }

  // cats defines Traverse on Tuple2, which is troublesome here because it means Transform
  // will ignore element 1 of a Tuple2 if this line is removed
  given transformTuple2[T1: Tag, T2: Tag](using =>Transformable[T1], =>Transformable[T2]): Transformable[Tuple2[T1, T2]] =
    derivedProduct[Tuple2[T1, T2]]

  given transformTraverse[T, F[_]: Traverse](using trans: =>Transformable[T])(using Tag[F[T]]): Transformable[F[T]] with {
    def doCombine[U](t: F[T], fn: [TT] => (TT => U) => Tag[TT] ?=> TT => U)(using Monoid[U]): U =
      fn[F[T]](_.foldMap(trans.doCombine(_, fn)))(t)

    def doRewrite(t: F[T], fn: [TT] => (TT => TT) => Tag[TT] ?=> TT => TT): F[T] =
      fn[F[T]](_.map(trans.doRewrite(_, fn)))(t)

    def doRewriteA[FF[_]](t: F[T], fn: [TT] => (TT => FF[TT]) => Tag[TT] ?=> TT => FF[TT])(using Applicative[FF]): FF[F[T]] =
      fn[F[T]](_.traverse(trans.doRewriteA[FF](_, fn)))(t)
  }

  given transformPrimitive[T: Tag: IsPrimitive]: Transformable[T] with {
    def doCombine[U](t: T, fn: [TT] => (TT => U) => Tag[TT] ?=> TT => U)(using Monoid[U]): U =
      fn[T](t => Monoid[U].empty)(t)

    def doRewrite(t: T, fn: [TT] => (TT => TT) => Tag[TT] ?=> TT => TT): T =
      fn[T](identity)(t)

    def doRewriteA[F[_]](t: T, fn: [TT] => (TT => F[TT]) => Tag[TT] ?=> TT => F[TT])(using Applicative[F]): F[T] =
      fn[T](_.pure)(t)
  }

  final class CombineBuilder[T: Transformable, U: Monoid](default: [TT] => (TT => U) => Tag[TT] ?=> TT => U) {
    def refine[I: Tag](fn: (I => U) => I => U): CombineBuilder[T, U] =
      CombineBuilder { [TT] => (defaultRec: (TT => U)) => (_: Tag[TT]) ?=> (tt: TT) =>
        default[TT] { tt =>
          if(Tag[TT] <:< Tag[I]) {
            fn(defaultRec.asInstanceOf[I => U])(tt.asInstanceOf[I])
          } else {
            defaultRec(tt)
          }
        }(tt)
      }

    def replace[I: Tag](fn: I => U): CombineBuilder[T, U] =
      refine[I](_ => fn)

    def incrementAt[I: Tag](using ev: U =:= Count)(pred: I => Boolean): CombineBuilder[T, U] =
      refine[I] { rec => i => 
        if(pred(i)) ev.flip(rec(i).inc) else rec(i)
      }

    def apply(t: T): U =
      Transformable[T].doCombine(t, default)
  }

  final class RewriteBuilder[T: Transformable](default: [TT] => (TT => TT) => Tag[TT] ?=> TT => TT) {
    def refine[I: Tag](fn: (I => I) => I => I): RewriteBuilder[T] =
      RewriteBuilder { [TT] => (defaultRec: (TT => TT)) => (_: Tag[TT]) ?=> (tt: TT) =>
        default[TT] { tt =>
          if(Tag[TT] <:< Tag[I]) {
            fn(defaultRec.asInstanceOf[I => I])(tt.asInstanceOf[I]).asInstanceOf[TT]
          } else {
            defaultRec(tt)
          }
        }(tt)
      }

    def replace[I: Tag](fn: I => I): RewriteBuilder[T] =
      refine[I](_ => fn)

    def apply(t: T): T =
      Transformable[T].doRewrite(t, default)
  }

  final class RewriteABuilder[T: Transformable, F[_]: Applicative](default: [TT] => (TT => F[TT]) => Tag[TT] ?=> TT => F[TT]) {
    def refine[I: Tag](fn: (I => F[I]) => I => F[I]): RewriteABuilder[T, F] =
      RewriteABuilder { [TT] => (defaultRec: (TT => F[TT])) => (_: Tag[TT]) ?=> (tt: TT) =>
        default[TT] { tt =>
          if(Tag[TT] <:< Tag[I]) {
            fn(defaultRec.asInstanceOf[I => F[I]])(tt.asInstanceOf[I]).asInstanceOf[F[TT]]
          } else {
            defaultRec(tt)
          }
        }(tt)
      }

    def replace[I: Tag](fn: I => F[I]): RewriteABuilder[T, F] =
      refine[I](_ => fn)

    def apply(t: T): F[T] =
      Transformable[T].doRewriteA[F](t, default)
  }
}
