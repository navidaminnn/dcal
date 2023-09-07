package distcompiler.dcal

import cats.data.*
import cats.*
//import cats.syntax.all.given
import distcompiler.transform.Transformable

import distcompiler.parsing.Ps

object Scoping {
  final case class ScopeBuilder(binders: Chain[AST.NameBinder], nestedScopes: Chain[(AST.ScopeRoot, ScopeBuilder)], references: Chain[AST.PathRef]) {
    def addNested(root: AST.ScopeRoot)(builder: ScopeBuilder): ScopeBuilder =
      copy(nestedScopes = nestedScopes :+ (root, builder))

    def addRef(ref: AST.PathRef): ScopeBuilder =
      copy(references = references :+ ref)

    def addBinder(binder: AST.NameBinder): ScopeBuilder =
      copy(binders = binders :+ binder)
  }

  object ScopeBuilder {
    val empty: ScopeBuilder = ScopeBuilder(
      binders = Chain.empty,
      nestedScopes = Chain.empty,
      references = Chain.empty,
    )

    given monoid: Monoid[ScopeBuilder] with {
      def empty: ScopeBuilder = ScopeBuilder.empty

      def combine(x: ScopeBuilder, y: ScopeBuilder): ScopeBuilder =
        ScopeBuilder(
          binders = x.binders ++ y.binders,
          nestedScopes = x.nestedScopes ++ y.nestedScopes,
          references = x.references ++ y.references,
        )
    }
  }

  def buildModuleScope(module: Ps[AST.Module]): ScopeBuilder =
    Transformable[Ps[AST.Module]]
      .combining[Id, ScopeBuilder]
      .refine[AST.ScopeRoot] { rec => root =>
        ScopeBuilder.empty.addNested(root)(rec(root))
      }
      .refine[AST.PathRef] { rec => ref =>
        rec(ref).addRef(ref)
      }
      .refine[AST.NameBinder] { rec => binder =>
        rec(binder).addBinder(binder)
      }
      .make
      .apply(module)
}
