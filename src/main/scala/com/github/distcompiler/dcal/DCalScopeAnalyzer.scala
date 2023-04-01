package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.{Expression, Statement}
import com.github.distcompiler.dcal.IRBuilder.{Context, NameInfo, ctx}

/**
 * Performs scope analysis on a DCalAST.Module against the following rules:
 * - Names must be in scope before they are referenced. A name is in scope if it is either a state variable, a
 *   declared local (using DCal var/let), a def name, or a module name.
 * - Additionally, all mutable locals must be initialized before they are referenced.
 * - A name must not be declared more than once in a scope. This also means that all state variable fields must be
 *   unique.
 *
 * There are other rules, for example typechecking rules, are reserved for another pass to handle. Namely:
 * - The bindings of all let and var statements must semi-type-check. This means that in a let/var statement, if the
 *   assignment operator is \`\in\`, then the binding must be a set.
 * - Operands in binary, relative, and logical operations must type-check.
 */
object DCalScopeAnalyzer {
  enum NameInfo {
    case MutableLocal // TODO: Removes later, because only var is a mutable local, but var compiles to be state.
    case Local
    case State
    case Definition
    case Module // names/defs of the current module do not need to be prefixed
    case ImportedModule // names/defs of imported modules need to be prefixed
  }

  inline def ctx(using ctx: Context): Context = ctx

  // TODO: Because Map is used, the same name cannot be used to refer to different NameInfos. Should this restriction
  //  be removed?
  final case class Context(outerCtx: Context, nameInfoOf: Map[String, NameInfo] = Map.empty) {
    def withNameInfo(name: String, nameInfo: NameInfo): Context =
      copy(nameInfoOf = nameInfoOf.updated(name, nameInfo))

    def withNameInfo[T](name: String, nameInfo: NameInfo)(fn: Context ?=> T): T =
      given Context = withNameInfo(name = name, nameInfo = nameInfo)
      fn

    def withOuterCtx(newCtx: Context): Context =
      copy(outerCtx = this)
  }

  private def analyzeExpression(dcalExpr: DCalAST.Expression)(using Context): Option[DCalErrors] = {
    dcalExpr match
      case Expression.True => None
      case Expression.False => None
      case Expression.IntLiteral(_) => None
      case Expression.StringLiteral(_) => None
      case Expression.Name(name) => ???
      case Expression.Set(members) => DCalErrors.union(members.map(analyzeExpression))
      case Expression.ExpressionBinOp(lhs, binOp, rhs) => ???
      case Expression.ExpressionRelOp(lhs, relOp, rhs) => ???
      case Expression.ExpressionLogicOp(lhs, logicOp, rhs) => ???
      case Expression.BracketedExpression(expression) => ???
  }

  private def analyzeStatement(dcalStmt: DCalAST.Statement)(using Context): Option[DCalErrors] = {
    dcalStmt match
      case Statement.Await(expression) => analyzeExpression(expression)
      // Loops over list of AssignPair
      // For each, checks that name is (a mutable local or) a state variable and calls analyzeExpression on expression
      case Statement.AssignPairs(assignPairs) => DCalErrors.union(assignPairs.map{ assignPair =>
        val nameErrorsOpt = ctx.nameInfoOf.get(assignPair.name) match
          case Some(NameInfo.State) => None
          case Some(_) => Some(DCalErrors(ReassignmentToImmutable(assignPair.name)))
          case None => Some(DCalErrors(NameNotFound(assignPair.name)))

        val expressionErrorsOpt = analyzeExpression(assignPair.expression)
        DCalErrors.union(nameErrorsOpt, expressionErrorsOpt)
      })
      case Statement.Let(name, assignmentOp, expression) => ???
      case Statement.Var(name, expressionOpt) => ???
      case Statement.IfThenElse(predicate, thenBlock, elseBlock) => ???
  }

  private def analyzeBlock(dcalBlock: DCalAST.Block)(using Context): Option[DCalErrors] =
    DCalErrors.union(dcalBlock.statements.map(analyzeStatement))

  private def analyzeDefinition(dcalDef: DCalAST.Definition)(using Context): Option[DCalErrors] = {
    // Adds params to ctx before analyzing def body
    val ctxWithParams = dcalDef.params.foldLeft(ctx)( (_ctx, param) =>
      _ctx.withNameInfo(param, NameInfo.Local)
    )
    analyzeBlock(dcalDef.body)(using ctxWithParams)
  }

  // TODO: search file systems, check if files exists and imported module names do not clash
  private def analyzeImport(dcalImport: List[String])(using Context): Option[DCalErrors] = None

  private def analyzeModule(dcalModule: DCalAST.Module)(using Context): Option[DCalErrors] = {
    // Adds module name to context before analyzing imports, so that analyzeImport can check for module name clashes
    val ctxWithModule = ctx.withNameInfo(dcalModule.name, NameInfo.Module)
    val importErrorsOpt = analyzeImport(dcalModule.imports)(using ctxWithModule)

    // Adds def names to context before analyzing body, because one def can refer to another
    val (ctxWithDefs, defNameErrorsOpt) =
      dcalModule.definitions.foldLeft((ctxWithModule, None: Option[DCalErrors]))( (acc, aDef) =>
        acc._1.nameInfoOf.get(aDef.name) match
          case None => (
            acc._1.withNameInfo(aDef.name, NameInfo.Definition),
            acc._2
          )
          case Some(NameInfo.Definition) => (
            acc._1,
            DCalErrors.union(acc._2, Some(DCalErrors(RedefinedName(aDef.name))))
          )
      )
    val bodyErrorsOpt = dcalModule.definitions.map { aDef => analyzeDefinition(aDef)(using ctxWithDefs) }

    // Returns the union of all errors from imports and body
    DCalErrors.union(importErrorsOpt::defNameErrorsOpt::bodyErrorsOpt)
  }

  def apply(dcalModule: DCalAST.Module): Option[DCalErrors] =
    analyzeModule(dcalModule)(using Context(
      outerCtx = null,
      nameInfoOf = Map[String, NameInfo](
        "str" -> NameInfo.State,
        "x" -> NameInfo.State,
        "y" -> NameInfo.State,
        "i" -> NameInfo.State,
        "set" -> NameInfo.State
      ))
    )
}
