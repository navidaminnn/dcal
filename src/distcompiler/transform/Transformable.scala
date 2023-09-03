package distcompiler.transform

import scala.compiletime.*
import scala.deriving.*
import scala.collection.mutable

import cats.*
import cats.data.*
import cats.syntax.all.given

import izumi.reflect.Tag

final class Transformable[T](private val transformFns: Transformable.TransformFns[T])(using val tag: Tag[T]) {
  private def makeCombine[U: Monoid](spliceFn: [TT] => Tag[TT] => (TT => Eval[U]) => TT => Eval[U]): T => U = {
    val cache = mutable.HashMap[Transformable[?], Any => Eval[U]]()
    val visited = mutable.HashSet[Transformable[?]]()

    given Transformable.CombineCtx[U] with {
      extension [T](self: Transformable[T]) def recurse: T => Eval[U] = impl(self)
    }

    def impl[T](self: Transformable[T]): T => Eval[U] =
      if(visited(self)) {
        t => cache(self).apply(t)
      } else {
        visited += self
        val result = spliceFn[T](self.tag)(self.transformFns.combine)
        cache.update(self, result.asInstanceOf[Any => Eval[U]])
        result
      }

    t => impl(this)(t).value
  }

  private def makeRewrite[F[_]: Applicative: Defer](spliceFn: [TT] => Tag[TT] => (TT => F[TT]) => TT => F[TT]): T => F[T] = {
    val cache = mutable.HashMap[Transformable[?], Any => F[Any]]()
    val visited = mutable.HashSet[Transformable[?]]()

    given Transformable.RewriteCtx[F] with {
      extension [T](self: Transformable[T]) def recurse: T => F[T] = impl(self)
    }

    def impl[T](self: Transformable[T]): T => F[T] =
      if(visited(self)) {
        t => cache(self).asInstanceOf[T => F[T]].apply(t)
      } else {
        visited += self
        val result = spliceFn[T](self.tag)(self.transformFns.rewrite)
        cache.update(self, result.asInstanceOf)
        result
      }

    impl(this)
  }

  def combining[U: Monoid]: Transformable.CombineBuilder[T, U] = {
    given Transformable[T] = this
    Transformable.CombineBuilder[T, U]([TT] => (_: Tag[TT]) => identity[TT => Eval[U]])
  }

  def rewriting[F[_]: Applicative: Defer]: Transformable.RewriteBuilder[T, F] = {
    given Transformable[T] = this
    Transformable.RewriteBuilder[T, F]([TT] => (_: Tag[TT]) => identity[TT => F[TT]])
  }
}

object Transformable extends TransformableLowPrio {
  trait CombineCtx[U] {
    extension [T] (self: Transformable[T]) def recurse: T => Eval[U]
  }

  trait RewriteCtx[F[_]] {
    extension [T] (self: Transformable[T]) def recurse: T => F[T]
  }

  trait TransformFns[T] {
    def combine[U: Monoid](using CombineCtx[U]): T => Eval[U]
    def rewrite[F[_]: Applicative: Defer](using RewriteCtx[F]): T => F[T]
  }

  def fromFns[T: Tag](transformFns: TransformFns[T]): Transformable[T] = new Transformable[T](transformFns)

  def apply[T](using trans: Transformable[T]): Transformable[T] = trans

  inline def derived[T: Tag]: Transformable[T] =
    summonFrom {
      case given Mirror.ProductOf[T & Product] => derivedProduct[T & Product].asInstanceOf[Transformable[T]]
      case mirror: Mirror.SumOf[T] => derivedSum[T, mirror.MirroredElemTypes](mirror.ordinal)
    }

  inline def derivedSum[T: Tag, Elems <: Tuple](ordinal: T => Int): Transformable[T] = {
    lazy val elemTransforms =
      summonAll[Tuple.Map[Elems, Transformable]]
        .productIterator
        .asInstanceOf[Iterator[Transformable[T]]]
        .toArray
    
    Transformable.fromFns(new TransformFns[T] {
      def combine[U](using Monoid[U], CombineCtx[U]): T => Eval[U] =
        t => Eval.defer(elemTransforms(ordinal(t)).recurse(t))

      def rewrite[F[_]](using Applicative[F], Defer[F], RewriteCtx[F]): T => F[T] =
        t => Defer[F].defer(elemTransforms(ordinal(t)).recurse(t))
    })
  }

  // cats defines Traverse on Tuple2, which is troublesome here because it means Transform
  // will ignore element 1 of a Tuple2 if this line is removed
  given transformTuple2[T1: Tag, T2: Tag](using =>Transformable[T1], =>Transformable[T2]): Transformable[Tuple2[T1, T2]] =
    derivedProduct[Tuple2[T1, T2]]

  given transformTraverse[T, F[_]: Traverse](using trans: =>Transformable[T])(using Tag[F[T]]): Transformable[F[T]] =
    Transformable.fromFns(new TransformFns[F[T]] {
      def combine[U](using Monoid[U], CombineCtx[U]): F[T] => Eval[U] =
        _.foldMap(trans.recurse)

      def rewrite[FF[_]](using Applicative[FF], Defer[FF], RewriteCtx[FF]): F[T] => FF[F[T]] =
        _.traverse(trans.recurse)
    })

  given transformPrimitive[T: Tag: IsPrimitive]: Transformable[T] =
    Transformable.fromFns(new TransformFns[T] {
      def combine[U](using Monoid[U], CombineCtx[U]): T => Eval[U] =
        _ => Eval.now(Monoid[U].empty)

      def rewrite[F[_]](using Applicative[F], Defer[F], RewriteCtx[F]): T => F[T] =
        Applicative[F].pure
    })

  final class CombineBuilder[T: Transformable, U: Monoid](default: [TT] => Tag[TT] => (TT => Eval[U]) => TT => Eval[U]) {
    def refineEval[I: Tag](fn: (I => Eval[U]) => I => Eval[U]): CombineBuilder[T, U] =
      CombineBuilder { [TT] => (tagTT: Tag[TT]) => 
        if(tagTT <:< Tag[I]) {
          (defaultRec: TT => Eval[U]) => fn.asInstanceOf[(TT => Eval[U]) => TT => Eval[U]](default[TT](tagTT)(defaultRec))
        } else {
          default[TT](tagTT)
        }
      }
    
    def refine[I: Tag](fn: (I => U) => I => U): CombineBuilder[T, U] =
      refineEval[I] { rec =>
        fn(rec.andThen(_.value)).andThen(Eval.now)
      }

    def replaceEval[I: Tag](fn: I => Eval[U]): CombineBuilder[T, U] =
      refineEval[I](_ => fn)

    def replace[I: Tag](fn: I => U): CombineBuilder[T, U] =
      refine[I](_ => fn)

    def incrementAt[I: Tag](using ev: U =:= Count)(pred: I => Boolean): CombineBuilder[T, U] =
      refineEval[I] { rec => i => 
        if(pred(i)) rec(i).map(c => ev.flip(ev(c).inc)) else rec(i)
      }

    def apply(t: T): U =
      make.apply(t)

    def make: T => U =
      Transformable[T].makeCombine(default)
  }

  final class RewriteBuilder[T: Transformable, F[_]: Applicative: Defer](default: [TT] => Tag[TT] => (TT => F[TT]) => TT => F[TT]) {
    def refine[I: Tag](fn: (I => F[I]) => I => F[I]): RewriteBuilder[T, F] =
      RewriteBuilder { [TT] => (tagTT: Tag[TT]) =>
        if(tagTT <:< Tag[I]) {
          (defaultRec: TT => F[TT]) => fn.asInstanceOf[(TT => F[TT]) => TT => F[TT]](default[TT](tagTT)(defaultRec))
        } else {
          default[TT](tagTT)
        }
      }

    def replace[I: Tag](fn: I => F[I]): RewriteBuilder[T, F] =
      refine[I](_ => fn)

    def apply(t: T): F[T] =
      make.apply(t)

    def make: T => F[T] =
      Transformable[T].makeRewrite(default)
  }
}

transparent trait TransformableLowPrio { self: Transformable.type =>
  inline given derivedProduct[T <: Product : Tag](using mirror: Mirror.ProductOf[T]): Transformable[T] = {
    lazy val elemTransforms =
      summonAll[Tuple.Map[mirror.MirroredElemTypes, Transformable]]
        .productIterator
        .asInstanceOf[Iterator[Transformable[Any]]]
        .toArray

    Transformable.fromFns(new TransformFns[T] {
      def combine[U](using Monoid[U], CombineCtx[U]): T => Eval[U] =
        { t =>
          (elemTransforms.iterator zip t.productIterator)
            .map((f, t) => Eval.defer(f.recurse(t)))
            .foldLeft(Monoid[Eval[U]].empty)(Monoid[Eval[U]].combine)
        }

      def rewrite[F[_]](using Applicative[F], Defer[F], RewriteCtx[F]): T => F[T] =
        { t =>
          Chain.traverseViaChain((elemTransforms.iterator zip t.productIterator).toIndexedSeq) {
            case (trans, t) => Defer[F].defer(trans.recurse(t))
          }.map { elems =>
            mirror.fromProduct(Tuple.fromArray(elems.iterator.toArray))
          }
        }
    })
  }
}
