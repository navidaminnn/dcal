package distcompiler.transform

import scala.deriving.*
import scala.collection.mutable

import cats.*
import cats.data.*
import cats.syntax.all.given

import izumi.reflect.Tag
import distcompiler.util.SummonTuple

final class Transformable[T](private val transformFns: Transformable.TransformFns[T], private val isSum: Boolean)(using val tag: Tag[T]) {
  private[Transformable] def markAsSum: Transformable[T] =
    new Transformable[T](transformFns, isSum = true)

  private def makeCombine[C[_]: Comonad, U: Monoid](spliceFn: [TT] => Option[Tag[TT]] => (C[TT] => Eval[U]) => C[TT] => Eval[U]): C[T] => U = {
    val cache = mutable.HashMap[Transformable[?], C[Any] => Eval[U]]()
    val visited = mutable.HashSet[Transformable[?]]()

    given Transformable.CombineCtx[C, U] with {
      extension [T](self: Transformable[T]) def recurse: C[T] => Eval[U] = impl(self)
    }

    def impl[T](self: Transformable[T]): C[T] => Eval[U] =
      if(visited(self)) {
        if(cache.contains(self)) {
          cache(self).asInstanceOf[C[T] => Eval[U]]
        } else {
          // if not in cache yet, make a new fn that looks in the cache later
          t => cache(self).apply(t.widen)
        }
      } else {
        visited += self
        val tagOpt = if(self.isSum) None else Some(self.tag)
        val result = spliceFn[T](tagOpt)(self.transformFns.combine)
        cache.update(self, result.asInstanceOf[C[Any] => Eval[U]])
        result
      }

    t => impl(this)(t).value
  }

  private def makeRewrite[C[_]: Comonad, F[_]: Applicative: Defer](spliceFn: [TT] => Tag[TT] => (C[TT] => F[TT]) => C[TT] => F[TT]): C[T] => F[T] = {
    val cache = mutable.HashMap[Transformable[?], C[Any] => F[Any]]()
    val visited = mutable.HashSet[Transformable[?]]()

    given Transformable.RewriteCtx[C, F] with {
      extension [T](self: Transformable[T]) def recurse: C[T] => F[T] = impl(self)
    }

    def impl[T](self: Transformable[T]): C[T] => F[T] =
      if(visited(self)) {
        if(cache.contains(self)) {
          cache(self).asInstanceOf[C[T] => F[T]]
        } else {
          t => cache(self).asInstanceOf[C[T] => F[T]].apply(t)
        }
      } else {
        visited += self
        val result = spliceFn[T](self.tag)(self.transformFns.rewrite)
        cache.update(self, result.asInstanceOf)
        result
      }

    impl(this)
  }

  def combining[C[_]: Comonad, U: Monoid]: Transformable.CombineBuilder[C, T, U] = {
    given Transformable[T] = this
    Transformable.CombineBuilder[C, T, U]([TT] => (_: Option[Tag[TT]]) => identity[C[TT] => Eval[U]])
  }

  def rewriting[C[_]: Comonad, F[_]: Applicative: Defer]: Transformable.RewriteBuilder[C, T, F] = {
    given Transformable[T] = this
    Transformable.RewriteBuilder[C, T, F]([TT] => (_: Tag[TT]) => identity[C[TT] => F[TT]])
  }
}

object Transformable extends TransformableLowPrio {
  trait CombineCtx[C[_], U] {
    extension [T] (self: Transformable[T]) def recurse: C[T] => Eval[U]
  }

  trait RewriteCtx[C[_], F[_]] {
    extension [T] (self: Transformable[T]) def recurse: C[T] => F[T]
  }

  trait TransformFns[T] {
    def combine[C[_]: Comonad, U: Monoid](using CombineCtx[C, U]): C[T] => Eval[U]
    def rewrite[C[_]: Comonad, F[_]: Applicative: Defer](using RewriteCtx[C, F]): C[T] => F[T]
  }

  def fromFns[T: Tag](transformFns: TransformFns[T]): Transformable[T] = new Transformable[T](transformFns, isSum = false)

  def apply[T](using trans: Transformable[T]): Transformable[T] = trans

  opaque type TransformableDerived[T] = Transformable[T]

  object TransformableDerived {
    given derivedSum[T: Tag](using mirror: Mirror.SumOf[T])(using elemTransforms: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, Transformable]]): TransformableDerived[T] =
      Transformable
        .fromFns(new TransformFns[T] {
          def combine[C[_], U](using Comonad[C], Monoid[U], CombineCtx[C, U]): C[T] => Eval[U] =
            t => Eval.defer(elemTransforms.value.productElement(mirror.ordinal(t.extract)).asInstanceOf[Transformable[T]].recurse(t))

          def rewrite[C[_], F[_]](using Comonad[C], Applicative[F], Defer[F], RewriteCtx[C, F]): C[T] => F[T] =
            t => Defer[F].defer(elemTransforms.value.productElement(mirror.ordinal(t.extract)).asInstanceOf[Transformable[T]].recurse(t))
        })
        .markAsSum

    given derivedProduct[T <: Product : Tag](using mirror: Mirror.ProductOf[T])(using elemTransforms: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, Transformable]]): TransformableDerived[T] =
      Transformable.derivedProduct[T]
  }

  def derived[T: Tag](using transformableDerived: TransformableDerived[T]): Transformable[T] =
    transformableDerived

  // cats defines Traverse on Tuple2, which is troublesome here because it means Transform
  // will ignore element 1 of a Tuple2 if this line is removed
  given transformTuple2[T1: Tag, T2: Tag](using =>Transformable[T1], =>Transformable[T2]): Transformable[Tuple2[T1, T2]] =
    derivedProduct[Tuple2[T1, T2]]

  given transformTraverse[T, F[_]: Traverse](using trans: =>Transformable[T])(using Tag[F[T]]): Transformable[F[T]] =
    Transformable.fromFns(new TransformFns[F[T]] {
      def combine[C[_], U](using Comonad[C], Monoid[U], CombineCtx[C, U]): C[F[T]] => Eval[U] =
        cft => cft.extract.foldMap(t => trans.recurse(cft.as(t)))

      def rewrite[C[_], FF[_]](using Comonad[C], Applicative[FF], Defer[FF], RewriteCtx[C, FF]): C[F[T]] => FF[F[T]] =
        cft => cft.extract.traverse(t => trans.recurse(cft.as(t)))
    })

  given transformPrimitive[T: Tag: IsPrimitive]: Transformable[T] =
    Transformable.fromFns(new TransformFns[T] {
      def combine[C[_], U](using Comonad[C], Monoid[U], CombineCtx[C, U]): C[T] => Eval[U] =
        _ => Eval.now(Monoid[U].empty)

      def rewrite[C[_], F[_]](using Comonad[C], Applicative[F], Defer[F], RewriteCtx[C, F]): C[T] => F[T] =
        ct => Applicative[F].pure(ct.extract)
    })

  final class CombineBuilder[C[_]: Comonad, T: Transformable, U: Monoid](default: [TT] => Option[Tag[TT]] => (C[TT] => Eval[U]) => C[TT] => Eval[U]) {
    def refineEval[I: Tag](fn: (C[I] => Eval[U]) => C[I] => Eval[U]): CombineBuilder[C, T, U] =
      CombineBuilder { [TT] => (tagTTOpt: Option[Tag[TT]]) =>
        if(tagTTOpt.exists(_ <:< Tag[I])) {
          (defaultRec: C[TT] => Eval[U]) => fn.asInstanceOf[(C[TT] => Eval[U]) => C[TT] => Eval[U]](default[TT](tagTTOpt)(defaultRec))
        } else {
          default[TT](tagTTOpt)
        }
      }
    
    def refine[I: Tag](fn: (C[I] => U) => C[I] => U): CombineBuilder[C, T, U] =
      refineEval[I] { rec =>
        fn(rec.andThen(_.value)).andThen(Eval.now)
      }

    def replaceEval[I: Tag](fn: C[I] => Eval[U]): CombineBuilder[C, T, U] =
      refineEval[I](_ => fn)

    def replace[I: Tag](fn: C[I] => U): CombineBuilder[C, T, U] =
      refine[I](_ => fn)

    def incrementAt[I: Tag](using ev: U =:= Count)(pred: C[I] => Boolean): CombineBuilder[C, T, U] =
      refineEval[I] { rec => i => 
        if(pred(i)) rec(i).map(c => ev.flip(ev(c).inc)) else rec(i)
      }

    def make: C[T] => U =
      Transformable[T].makeCombine(default)
  }

  final class RewriteBuilder[C[_]: Comonad, T: Transformable, F[_]: Applicative: Defer](default: [TT] => Tag[TT] => (C[TT] => F[TT]) => C[TT] => F[TT]) {
    def refine[I: Tag](fn: (C[I] => F[I]) => C[I] => F[I]): RewriteBuilder[C, T, F] =
      RewriteBuilder { [TT] => (tagTT: Tag[TT]) =>
        if(tagTT <:< Tag[I]) {
          (defaultRec: C[TT] => F[TT]) => fn.asInstanceOf[(C[TT] => F[TT]) => C[TT] => F[TT]](default[TT](tagTT)(defaultRec))
        } else {
          default[TT](tagTT)
        }
      }

    def replace[I: Tag](fn: C[I] => F[I]): RewriteBuilder[C, T, F] =
      refine[I](_ => fn)

    def make: C[T] => F[T] =
      Transformable[T].makeRewrite(default)
  }
}

transparent trait TransformableLowPrio { self: Transformable.type =>
  given derivedProduct[T <: Product : Tag](using mirror: Mirror.ProductOf[T])(using elemTransforms: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, Transformable]]): Transformable[T] =
    Transformable.fromFns(new TransformFns[T] {
      private def elemTransformsIter: Iterator[Transformable[Any]] =
        elemTransforms.value.productIterator.asInstanceOf[Iterator[Transformable[Any]]]

      def combine[C[_], U](using Comonad[C], Monoid[U], CombineCtx[C, U]): C[T] => Eval[U] =
        { t =>
          (elemTransformsIter zip t.extract.productIterator)
            .map((f, tt) => Eval.defer(f.recurse(t.as(tt))))
            .foldLeft(Monoid[Eval[U]].empty)(Monoid[Eval[U]].combine)
        }

      def rewrite[C[_], F[_]](using Comonad[C], Applicative[F], Defer[F], RewriteCtx[C, F]): C[T] => F[T] =
        { t =>
          Chain.traverseViaChain((elemTransformsIter zip t.extract.productIterator).toIndexedSeq) {
            case (trans, tt) => Defer[F].defer(trans.recurse(t.as(tt)))
          }.map { elems =>
            mirror.fromProduct(Tuple.fromArray(elems.iterator.toArray))
          }
        }
    })
}
