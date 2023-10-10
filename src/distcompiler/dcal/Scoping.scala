package distcompiler.dcal

import scala.compiletime.asMatchable

import cats.*
import cats.data.*
// import cats.derived.*
import cats.syntax.all.given

import distcompiler.parsing.{Ps, SourceLocation, SourceLocated}
import distcompiler.transform.Poly
// import distcompiler.util.!!!

final case class Scoped[T](value: Ps[T], info: Scoped.Info) extends SourceLocated {
  export value.sourceLocation

  def extract: T = value.extract
}

object Scoped {
  object AST extends ASTF[Scoped]

  type RootPath = List[IdAST.ScopeRoot]

  final case class Info(rootPath: RootPath, refs: Map[(Scoped.RootPath, IdAST.Node), AST.NameBinderT])

  abstract class scopingImpl[F[_] <: SourceLocated : Comonad](val src: ASTF[F], val ctx: Unit) extends Poly, Poly.IdentityA, Poly.GenericTransformA, Poly.TraverseTransformA {
    // replace Scoped with an enum.
    // cases:
    // - root
    // - other?

    // Eval[Scoped[T]] should be applicative, so that it can be made with `pure` and threaded through the traversal

    // ctx should contain all binders in scope, as well as the RootPath that qualifies binders

    // typing will use scope info and the uniqueness of a binder + root path to maintain a monoidal map of all known types
    // types that are not monoidal... would we have any? we have values (w/ sets of ops), interface instances (where we must always be able to make dispatch, including when to drop)
    // ... the drop part is not monoidal, but I think the rest all is. rest should all be doable as "disagreements over shape"

    given scopePs[T, U](using subCase: =>Case[T, Eval[U]]): GeneralCase[Ps[T], Eval[Scoped[U]]] with {
      def apply(t: Ps[T]): Eval[Scoped[U]] =
        subCase(t.extract).map { u =>
          ???
        }
    }

    given scopeNonEmptyList[T, U](using subCase: =>Case[T, Eval[U]]): GeneralCase[NonEmptyList[T], Eval[NonEmptyList[U]]] with {
      def apply(t: NonEmptyList[T]): Eval[NonEmptyList[U]] =
        traverseTransformACase[NonEmptyList, Eval, T, U](t)
    }

    given scopeSourceLocation: GeneralCase[SourceLocation, Eval[SourceLocation]] with {
      def apply(t: SourceLocation): Eval[SourceLocation] = Eval.now(t)
    }

    given scopeExpressionT: GeneralCase[src.ExpressionT, Eval[AST.ExpressionT]] =
      scopePs[src.Expression, AST.Expression]
    
    // given scopeNode[T <: src.NodeT, U <: ScopedAST.NodeT](using rec: =>GeneralCase[T, Eval[U]]): SpecialCase[T, Eval[U]] with {
    //   def apply(t: T): Eval[U] =
    //     t.extract match {
    //       case n: src.ScopeRoot =>
    //         ???
    //       case n: src.PathRef =>
    //         ???
    //       case n: src.NameBinder =>
    //         ???
    //       case _ =>
    //         ???
    //     }
    // }
  }

  // object scopeParsedAST extends scopingImpl[Ps](ParsedAST, ())

  // scopeParsedAST.apply[ParsedAST.Statements, Eval[ScopedAST.Statements]](???)

  //scopeParsedAST.apply[ParsedAST.Definition.Qualifier, Eval[ScopedAST.Definition.Qualifier]](???)
  // scopeParsedAST.apply[ParsedAST.Definition.Qualifier, Eval[ScopedAST.Definition.Qualifier]](???)
  //scopeParsedAST.apply[ParsedAST.Expression, Eval[ScopedAST.Expression]](???)
  //scopeParsedAST.apply[ParsedAST.ExpressionT, Eval[ScopedAST.ExpressionT]](???)
  //scopeParsedAST.apply[ParsedAST.Definition, Eval[ScopedAST.Definition]](???)

  // def checkIntegrity[T: Transformable](root: T): ValidatedNec[ScopingError, Unit] =
  //   Transformable[T]
  //     .combining[Scoping, ValidatedNec[ScopingError, Unit]]
  //     .refineEval[Ps[AST.ScopeRoot]] { rec => scopeRootScope =>
  //       Eval.now {
  //         scopeRootScope
  //           .scopingInfo
  //           .binders
  //           .flatMap(b => b.value.boundNames.iterator.map(_ -> b))
  //           .groupMap(_._1)(_._2)
  //           .toList
  //           .sortBy(_._1.sourceLocation.offsetStart)
  //           .foldMap {
  //             case (_, Nil) => !!! // groupMap should never produce that?
  //             case (_, _ :: Nil) => ().validNec
  //             case (_, first :: dups) =>
  //               dups.foldMap(dup => ScopingError.Redefinition(first, dup).invalidNec[Unit])
  //           }
  //       }
  //         .productR(rec(scopeRootScope))
  //     }
  //     .refineEval[Ps[AST.PathRef]] { rec => refScope =>
  //       val ref = refScope.extract
  //       Eval.now(refScope.scopingInfo.lookup_?(ref.value.refPath.value))
  //         .productR(rec(refScope))
  //     }
  //     .make
  //     .apply(Scoping.fromRoot(root))

  given comonad: Comonad[Scoped] with {
    def extract[A](x: Scoped[A]): A =
      x.extract
    
    def coflatMap[A, B](fa: Scoped[A])(f: Scoped[A] => B): Scoped[B] =
      fa.coflatMap(f)

    def map[A, B](fa: Scoped[A])(f: A => B): Scoped[B] =
      coflatMap(fa)(fa => f(extract(fa)))
  }

  // final case class Info(parent: Option[Info], root: Option[AST.ScopeRoot], binders: List[Ps[AST.NameBinder]], references: List[Ps[AST.PathRef]]) {
  //   val rootPath: RootPath =
  //     root.fold(Nil)(_ :: parent.fold(Nil)(_.rootPath))

  //   val byName: Map[Ps[AST.PathSegment], Ps[AST.NameBinder]] =
  //     parent.fold(Map.empty)(_.byName)
  //     ++ binders.iterator.flatMap(binder => binder.value.boundNames.iterator.map(_ -> binder))

  //   def lookup_?(path: AST.Path): ValidatedNec[ScopingError, Ps[AST.NameBinder]] =
  //     path.parts match {
  //       case NonEmptyList(head, _) =>
  //         byName.get(head) match {
  //           case None => ScopingError.UndefinedReference(head).invalidNec
  //           case Some(binder) => binder.validNec
  //         }
  //     }
  // }

  // object Info {
  //   val empty: Info = Info(parent = None, root = None, binders = Nil, references = Nil)

  //   final case class Builder(binders: Chain[Ps[AST.NameBinder]], references: Chain[Ps[AST.PathRef]]) derives Monoid {
  //     def withBinder(binder: Ps[AST.NameBinder]): Builder =
  //       copy(binders = binders :+ binder)

  //     def withReference(ref: Ps[AST.PathRef]): Builder =
  //       copy(references = references :+ ref)
  //   }

  //   def fromScopeRoot(parent: Option[Info], root: AST.ScopeRoot): Info = {
  //     val builder =
  //       Transformable[AST.ScopeRoot]
  //         .combining[Id, Builder]
  //         .refineEval[AST.ScopeRoot](rec => {
  //           case r if r eq root => rec(root)
  //           case root => Eval.now(Monoid[Builder].empty)
  //         })
  //         .refineEval[Ps[AST.NameBinder]] { rec => binder =>
  //           rec(binder).map(_.withBinder(binder))
  //         }
  //         .refineEval[Ps[AST.PathRef]] { rec => ref =>
  //           rec(ref).map(_.withReference(ref))
  //         }
  //         .make
  //         .apply(root)

  //     Info(
  //       parent = parent,
  //       root = Some(root),
  //       binders = builder.binders.toList,
  //       references = builder.references.toList,
  //     )
  //   }
  // }
}
