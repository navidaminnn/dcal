package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.{Expression, Statement}

/**
 * Performs scope analysis on a DCalAST.Module against the following rules:
 * - Names must be in scope before they are referenced. A name is in scope if it is either a state variable, a
 *   declared local (using DCal var/let), a def name, or a module name.
 * - A name must not be declared more than once in a scope, even if the redeclaration is done for a different name type.
 *
 * There are other rules, for example typechecking rules, are reserved for another pass to handle. Namely:
 * - The bindings of all let and var statements must semi-type-check. This means that in a let/var statement, if the
 *   assignment operator is \`\in\`, then the binding must be a set.
 * - Operands in binary, relative, and logical operations must type-check.
 */
object DCalScopeAnalyzer {
  enum NameInfo {
    case Local
    case State
    case Definition
    case Module // names/defs of the current module do not need to be prefixed
    case ImportedModule // names/defs of imported modules need to be prefixed
  }

  inline def ctx(using ctx: Context): Context = ctx

  final case class Context(nameInfoOf: Map[String, NameInfo] = Map.empty) {
    def withNameInfo(name: String, nameInfo: NameInfo): Context =
      copy(nameInfoOf = nameInfoOf.updated(name, nameInfo))

    def withNameInfo[T](name: String, nameInfo: NameInfo)(fn: Context ?=> T): T =
      given Context = withNameInfo(name = name, nameInfo = nameInfo)
      fn
  }

  private def analyzeExpression(dcalExpr: DCalAST.Expression)(using Context): Option[DCalErrors] = {
    dcalExpr match
      case Expression.True | Expression.False | Expression.IntLiteral(_) | Expression.StringLiteral(_) => None
      case Expression.Name(name) => ctx.nameInfoOf.get(name) match {
        case Some(_) => None
        case None => Some(DCalErrors(NameNotFound(name)))
      }
      case Expression.Set(members) => DCalErrors.union(members.map(analyzeExpression))
      case Expression.ExpressionBinOp(lhs, _, rhs) => DCalErrors.union(analyzeExpression(lhs), analyzeExpression(rhs))
      case Expression.ExpressionRelOp(lhs, _, rhs) => DCalErrors.union(analyzeExpression(lhs), analyzeExpression(rhs))
      case Expression.ExpressionLogicOp(lhs, _, rhs) => DCalErrors.union(analyzeExpression(lhs), analyzeExpression(rhs))
      case Expression.BracketedExpression(expression) => analyzeExpression(expression)
  }

  // For statements that declare and/or define a name, if that name already exists
  private def analyzeStatement(dcalStmt: DCalAST.Statement)(using Context): Option[DCalErrors] = {
    dcalStmt match
      case Statement.Await(expression) => analyzeExpression(expression)

      // Loops over list of AssignPair
      // For each, checks that name is (a mutable local or) a state variable and calls analyzeExpression on expression
      case Statement.AssignPairs(assignPairs) => DCalErrors.union(assignPairs.map{ assignPair =>
        val nameErrsOpt = ctx.nameInfoOf.get(assignPair.name) match
          case Some(NameInfo.State) => None
          case Some(_) => Some(DCalErrors(ReassignmentToImmutable(assignPair.name)))
          case None => Some(DCalErrors(NameNotFound(assignPair.name)))
        val expressionErrsOpt = analyzeExpression(assignPair.expression)
        DCalErrors.union(nameErrsOpt, expressionErrsOpt)
      })

      case Statement.Let(name, _, expression) =>
        // Check that name has not been declared
        val nameErrsOpt = ctx.nameInfoOf.get(name) match
          case Some(_) => Some(DCalErrors(RedeclaredName(name)))
          case None => None
        val exprErrsOpt = analyzeExpression(expression)
        DCalErrors.union(nameErrsOpt, exprErrsOpt)

      case Statement.Var(name, expressionOpt) =>
        val nameErrsOpt = ctx.nameInfoOf.get(name) match
          case Some(_) => Some(DCalErrors(RedeclaredName(name)))
          case None => None
        val exprErrsOpt = expressionOpt.map {
          case (_, binding) => analyzeExpression(binding)
        }.getOrElse(None)
        DCalErrors.union(nameErrsOpt, exprErrsOpt)

      case Statement.IfThenElse(predicate, thenBlock, elseBlock) =>
        DCalErrors.union(List(analyzeExpression(predicate), analyzeBlock(thenBlock), analyzeBlock(elseBlock)))
  }

  private def analyzeBlock(dcalBlock: DCalAST.Block)(using Context): Option[DCalErrors] = {
    def analyzeStatements(stmts: List[DCalAST.Statement], errsOpt: Option[DCalErrors])(using Context): Option[DCalErrors] = {
      stmts match
        case Nil => errsOpt
        // Analyzes head, creates a new ctx if necessary, recurses on tail
        case s::ss =>
          val sErrs = analyzeStatement(s)
          s match
            case Statement.Let(name, _, _) => ctx.withNameInfo(name, NameInfo.Local){
              analyzeStatements(ss, DCalErrors.union(errsOpt, sErrs))
            }
            case Statement.Var(name, _) => ctx.withNameInfo(name, NameInfo.State){
              analyzeStatements(ss, DCalErrors.union(errsOpt, sErrs))
            }
            case _ => analyzeStatements(ss, DCalErrors.union(errsOpt, sErrs))
    }
    analyzeStatements(dcalBlock.statements, None: Option[DCalErrors])
  }

  private def analyzeDefinition(dcalDef: DCalAST.Definition)(using Context): Option[DCalErrors] = {
    // Adds params to ctx before analyzing def body
    val ctxWithParams = dcalDef.params.foldLeft(ctx)( (_ctx, param) =>
      _ctx.withNameInfo(param, NameInfo.Local)
    )
    analyzeBlock(dcalDef.body)(using ctxWithParams)
  }

  // TODO: Searches file systems, check if files exists and imported module names do not clash.
  private def analyzeImport(dcalImport: List[String])(using Context): Option[DCalErrors] = None

  private def analyzeModule(dcalModule: DCalAST.Module)(using Context): Option[DCalErrors] = {
    // Adds module name to context before analyzing imports, so that analyzeImport can check for module name clashes
    val ctxWithModule = ctx.withNameInfo(dcalModule.name, NameInfo.Module)

    // TODO: Adds import names to context before analyzing body, so that body can refer to import names.
    val importErrsOpt = analyzeImport(dcalModule.imports)(using ctxWithModule)

    // Adds def names to context before analyzing body, because one def can refer to another
    val (ctxWithDefs, defNameErrsOpt) =
      dcalModule.definitions.foldLeft((ctxWithModule, None: Option[DCalErrors]))( (acc, aDef) =>
        acc._1.nameInfoOf.get(aDef.name) match
          case None => (
            acc._1.withNameInfo(aDef.name, NameInfo.Definition),
            acc._2
          )
          case Some(_) => (
            acc._1,
            DCalErrors.union(acc._2, Some(DCalErrors(RedeclaredName(aDef.name))))
          )
      )
    val bodyErrsOpt = dcalModule.definitions.map { aDef => analyzeDefinition(aDef)(using ctxWithDefs) }

    // Returns the union of all errors from imports and body
    DCalErrors.union(importErrsOpt::defNameErrsOpt::bodyErrsOpt)
  }

  def apply(dcalModule: DCalAST.Module): Option[DCalErrors] =
    analyzeModule(dcalModule)(using Context(
      nameInfoOf = Map[String, NameInfo](
        "str" -> NameInfo.State,
        "x" -> NameInfo.State,
        "y" -> NameInfo.State,
        "i" -> NameInfo.State,
        "set" -> NameInfo.State
      ))
    )
}
