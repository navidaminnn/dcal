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
      .combining[ScopeBuilder]
      .refine[AST.ScopeRoot] { rec => root =>
        ScopeBuilder.empty.addNested(root)(rec(root))
      }
      .refine[AST.PathRef] { rec => ref =>
        rec(ref).addRef(ref)
      }
      .refine[AST.NameBinder] { rec => binder =>
        rec(binder).addBinder(binder)
      }
      .apply(module)

  // import AST.*

  // enum Dir[+T] {
  //   case Direct(ref: PsK[T])
  //   case Imported(path: PsK[Path], ref: Dir[T])
  //   case Extended(path: PsK[Path], ref: Dir[T])
  //   case ViaImplParam(param: PsK[Param.Impl], ref: Dir[T])
  // }

  // enum Reference {
  //   case OpCall(opCall: PsK[Expression.OpCall], varDefn: Dir[Statement.Var | Statement.Let | Param.Name])
  //   case DefCall(call: PsK[Statement.Call | Binding.Call], defn: Dir[Definition.Def])
  //   case Implements(impl: PsK[Definition.Impl | Param.Impl], iface: Dir[Definition.Interface])
  //   case Instantiates(inst: PsK[CallArg.Instance | Statement.Instance], impl: Dir[Definition.Impl])
  //   case Aliases(alias: PsK[CallArg.Alias], inst: PsK[Statement.Instance | Param.Impl])
  // }

  // final case class ScopingContext(defs: AvlMap[String, Ps[Def]]) {
  //   def withDefs(defs: AvlMap[String, Ps[Def]]): ScopingContext =
  //     copy(defs = this.defs ++ defs)

  //   def lookup(name: Path): Option[Ps[Def]] =
  //     name match {
  //       case Path.Name(name) => defs.get(name)
  //       case _ => None // TODO: actually implement this part
  //     }
  // }
  // object ScopingContext {
  //   val empty: ScopingContext = ScopingContext(defs = AvlMap.empty)
  // }

  // private inline def ctx(using ctx: ScopingContext): ScopingContext = ctx

  // type ReferencePair = (PsK[Referent], PsK[Def])
  // type ScopingInfo = ValidatedNec[ScopingError, Chain[ReferencePair]]

  // private given stringIsEmpty: Transform[String, Eval[ScopingInfo]] =
  //   Transform.fromFunction(_ => Eval.now(Chain.nil.valid))

  // private given bigIntIsEmpty: Transform[BigInt, Eval[ScopingInfo]] =
  //   Transform.fromFunction(_ => Eval.now(Chain.nil.valid))

  // def scopeModule(module: Module)(using ScopingContext): ScopingInfo = {
  //   val Module(name, imports, definitions) = module
  //   //assert(imports.isEmpty)// TODO: fixme

  //   val topLevelDefs: AvlMap[String, Chain[Ps[Def]]] =
  //     definitions
  //       .map {
  //         case defn @ Ps(Definition(Ps(name), _, _)) => name -> Chain.one(defn.widen[Def])
  //       }
  //       .toCatsMultiMap

  //   topLevelDefs
  //     .toList
  //     .map(_._2)
  //     .collectFold[ScopingInfo] {
  //       case firstDefn ==: redefinitions =>
  //         redefinitions.foldMap[ScopingInfo] { redefinition =>
  //           ScopingError.Redefinition(firstDefn.toPsK, redefinition.toPsK).invalidNec
  //         }
  //     }
  //     .andThen { _ =>
  //       val topLevelDefsNoDups = topLevelDefs.map(_.headOption.get)
  //       definitions.foldMap(scopeDefinition(_)(using ctx.withDefs(topLevelDefsNoDups)))
  //     }
  // }

  // def scopeDefinition(defn: Ps[Definition])(using ScopingContext): ScopingInfo = {
  //   val Ps(Definition(name, arguments, body)) = defn

  //   val argDefs: AvlMap[String, Chain[Ps[Def]]] =
  //     arguments
  //       .map {
  //         case ps @ Ps(DefParam.Name(name)) => name -> Chain.one(ps.widen[Def])
  //       }
  //       .toCatsMultiMap

  //   argDefs
  //     .toList
  //     .map(_._2)
  //     .collectFold[ScopingInfo] {
  //       case firstDef ==: redefinitions =>
  //         redefinitions.foldMap[ScopingInfo] { redefinition =>
  //           ScopingError.Redefinition(firstDef.toPsK.widen, redefinition.toPsK.widen).invalidNec
  //         }
  //     }
  //     .andThen { _ =>
  //       val argDefsNoDups = argDefs.map(_.headOption.get)
  //       val outerCtx = ctx
  //       locally {
  //         given ScopingContext = outerCtx.withDefs(argDefsNoDups)
  //         summon[Transform[Eval[Ps[Statement]], Eval[ScopingInfo]]](Eval.now(body)).value
  //       }
  //     }
  // }

  // private def scopeCall(from: Ps[Referent], call: Binding.Call)(using ScopingContext): Eval[ScopingInfo] = {
  //   val Binding.Call(path, arguments) = call

  //   Eval.now[ScopingInfo] {
  //     ctx.lookup(path.value) match {
  //       case None =>
  //         ScopingError.UndefinedReference(from.toPsK.widen).invalidNec
  //       case Some(defn @ Ps(Definition(_, params, _))) =>
  //         if(params.size != arguments.size) {
  //           ScopingError.ArityMismatch(from.toPsK.widen, defn.toPsK.widen).invalidNec
  //         } else {
  //           Chain.one(from.toPsK.widen -> defn.toPsK.widen).valid
  //         }
  //       case Some(otherDef) =>
  //         ScopingError.KindMismatch(from.toPsK.widen, otherDef.toPsK.widen).invalidNec
  //     }
  //   }
  //   `combine` summon[Transform[Eval[Ps[Path]], Eval[ScopingInfo]]](Eval.now(path))
  //   `combine` arguments.foldMap(scopeExpression.asFunction)
  // }

  // given scopeBinding(using ScopingContext): Transform.Refined[Ps[Binding], Eval[ScopingInfo]] =
  //   Transform.Refined.fromPartialFunction {
  //     case from @ Ps(call @ Binding.Call(path, arguments)) =>
  //       scopeCall(from.as(call), call)
  //   }

  // given scopeExpression(using ScopingContext): Transform.Refined[Ps[Expression], Eval[ScopingInfo]] =
  //   Transform.Refined.fromPartialFunction {
  //     case from @ Ps(call @ Expression.OpCall(ident, arguments)) =>
  //       val fromCallK = from.as(call).toPsK
  //       val id = 
  //         ident match {
  //           case Left(op) => ???
  //           case Right(path) => path
  //         }
        
  //       Eval.now[ScopingInfo] {
  //         ctx.lookup(id.value) match {
  //           case None =>
  //             ScopingError.UndefinedReference(fromCallK).invalidNec
  //           case Some(defn @ Ps(DefParam.Name(_) | Statement.Let(_, _) | Statement.Var(_, _))) =>
  //             if(arguments.isEmpty) {
  //               Chain.one(fromCallK -> defn.toPsK).valid
  //             } else {
  //               ScopingError.ArityMismatch(fromCallK, defn.toPsK).invalidNec
  //             }
  //           case Some(defn @ Ps(_: Definition | Import.Name(_))) =>
  //             ScopingError.KindMismatch(fromCallK, defn.toPsK).invalidNec
  //         }
  //       }
  //       `combine` summon[Transform[Either[Ps[BinaryOperator], Ps[Path]], Eval[ScopingInfo]]](ident)
  //       `combine` arguments.foldMap(scopeExpression.asFunction)
  //   }

  // given scopeStatement(using ScopingContext): Transform.Refined[Ps[Statement], Eval[ScopingInfo]] =
  //   Transform.Refined.fromPartialFunction {
  //     case from @ Ps(stmtCall @ Statement.Call(Ps(call))) =>
  //       scopeCall(from.as(stmtCall).widen, call)
  //   }

  // given scopeStatements(using ScopingContext): Transform.Refined[List[Ps[Statement]], Eval[ScopingInfo]] = {
  //   def scopeLetVar(name: String, binding: Ps[Binding], defn: Ps[Def], restStmts: List[Ps[Statement]]): Eval[ScopingInfo] = {
  //     val newCtx = ctx.withDefs(AvlMap(name -> defn))

  //     scopeBinding(binding)
  //     `combine` restStmts.foldMap(scopeStatement(using newCtx).asFunction)
  //   }

  //   Transform.Refined.fromPartialFunction[List[Ps[Statement]], Eval[ScopingInfo]] {
  //     case (defn @ Ps(d @ Statement.Let(name, binding))) :: restStmts =>
  //       scopeLetVar(name.value, binding, defn.map(_ => d), restStmts)
  //     case (defn @ Ps(d @ Statement.Var(name, binding))) :: restStmts =>
  //       scopeLetVar(name.value, binding, defn.map(_ => d), restStmts)
  //   }
  // }
}
