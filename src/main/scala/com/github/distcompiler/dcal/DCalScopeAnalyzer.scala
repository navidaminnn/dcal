package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.{Expression, Statement}

/**
 * Performs scope analysis on a DCalAST.Module against the following rules:
 * - Names must be in scope before they are referenced. A name is in scope if it is either a state variable, a
 *   declared local (using DCal var/let), a def name, or a module name.
 * - Additionally, all mutable locals must be initialized before they are referenced.
 * - A name must not be declared more than once in a scope. This also means that all state variable fields must be
 *   unique.
 * - The bindings of all let and var statements must type-check. This means that in a let/var statement, if the
 *   assignment operator is \`\in\`, then the binding must be a set.
 * - Operands in binary, relative, and logical operations must type-check.
 */
object DCalScopeAnalyzer {
  enum NameInfo {
    case MutableLocal
    case ImmutableLocal
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

    def withOuterCtx(newCtx: Context): Context =
      copy(outerCtx = this)
  }

  private def analyzeExpression(dcalExpr: DCalAST.Expression)(using Context): Option[DCalErrors] = {
    dcalExpr match
      case Expression.True => ???
      case Expression.False => ???
      case Expression.IntLiteral(value) => ???
      case Expression.StringLiteral(value) => ???
      case Expression.Name(name) => ???
      case Expression.Set(members) => ???
      case Expression.ExpressionBinOp(lhs, binOp, rhs) => ???
      case Expression.ExpressionRelOp(lhs, relOp, rhs) => ???
      case Expression.ExpressionLogicOp(lhs, logicOp, rhs) => ???
      case Expression.BracketedExpression(expression) => ???
  }

  private def analyzeStatement(dcalStmt: DCalAST.Statement)(using Context): Option[DCalErrors] = {
    dcalStmt match
      case Statement.Await(expression) => analyzeExpression(expression)
      case Statement.AssignPairs(assignPairs) => ???
      case Statement.Let(name, assignmentOp, expression) => ???
      case Statement.Var(name, expressionOpt) => ???
      case Statement.IfThenElse(predicate, thenBlock, elseBlock) => ???
  }

  private def analyzeBlock(dcalBlock: DCalAST.Block)(using Context): Option[DCalErrors] =
    DCalErrors.union(dcalBlock.statements.map(analyzeStatement))

  private def analyzeDefinition(dcalDef: DCalAST.Definition)(using Context): Option[DCalErrors] = {
    // Checks if def name clashes
    val defNameErrors = ctx.nameInfoOf.get(dcalDef.name) match
      case Some(NameInfo.Definition) => Some(DCalErrors(List(RedefinedName(dcalDef.name))))
      case _ => None

    // Adds params to ctx before analyzing def body
    val ctxWithParams = dcalDef.params.foldLeft(ctx)( (_ctx, param) =>
      _ctx.withNameInfo(param, NameInfo.ImmutableLocal)
    )
    val bodyErrors = analyzeBlock(dcalDef.body)(using ctxWithParams)

    // Returns the union of defNameErrors and bodyErrors
    DCalErrors.union(defNameErrors, bodyErrors)
  }

  // TODO: search file systems, check if files exists and imported module names do not clash
  private def analyzeImport(dcalImport: List[String])(using Context): Option[DCalErrors] = ???

  private def analyzeModule(dcalModule: DCalAST.Module)(using Context): Option[DCalErrors] = {
    // Adds module name to context before analyzing imports, so that analyzeImport can check for module name clashes
    val ctxWithModule = ctx.withNameInfo(dcalModule.name, NameInfo.Module)
    val importErrorsOpt = analyzeImport(dcalModule.imports)(using ctxWithModule)

    // Adds def names to context before analyzing body
    val ctxWithDefs = dcalModule.definitions.foldLeft(ctxWithModule)( (_ctx, aDef) =>
      _ctx.withNameInfo(aDef.name, NameInfo.Definition)
    )
    val bodyErrorsOpt = DCalErrors.union(
      dcalModule.definitions.map { aDef => analyzeDefinition(aDef)(using ctxWithDefs) }
    )

    // Returns the union of all errors from imports and body
    DCalErrors.union(importErrorsOpt, bodyErrorsOpt)
  }

  def apply(dcalModule: DCalAST.Module): Option[DCalErrors] =
    analyzeModule(dcalModule)(using Context(outerCtx = null))
}
