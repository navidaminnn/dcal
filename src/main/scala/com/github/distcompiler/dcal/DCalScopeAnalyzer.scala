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
    case Module
  }

  inline def ctx(using ctx: Context): Context = ctx

  final case class Context(nameInfoOf: Map[String, NameInfo] = Map.empty,
                           defsOf: Map[String, List[String]] = Map.empty) {
    def withNameInfo(name: String, nameInfo: NameInfo): Context =
      copy(nameInfoOf = nameInfoOf.updated(name, nameInfo))

    def withNameInfo[T](name: String, nameInfo: NameInfo)(fn: Context ?=> T): T =
      given Context = withNameInfo(name = name, nameInfo = nameInfo)
      fn

    def withImport(importName: String, defs: List[String]) =
      copy(defsOf = defsOf.updated(importName, defs))
  }

  private val specDir = os.pwd / "src" / "test" / "resources" / "DCal"

  private def analyzeExpression(dcalExpr: DCalAST.Expression)(using Context): DCalErrors = {
    dcalExpr match
      case Expression.True | Expression.False | Expression.IntLiteral(_) | Expression.StringLiteral(_) => DCalErrors(errors = Nil)
      case Expression.Name(name) => ctx.nameInfoOf.get(name) match {
        case Some(_) => DCalErrors(errors = Nil)
        case None => DCalErrors(NameNotFound(name))
      }
      case Expression.Set(members) => DCalErrors.union(members.map(analyzeExpression))
      case Expression.ExpressionBinOp(lhs, _, rhs) => DCalErrors.union(analyzeExpression(lhs), analyzeExpression(rhs))
      case Expression.ExpressionRelOp(lhs, _, rhs) => DCalErrors.union(analyzeExpression(lhs), analyzeExpression(rhs))
      case Expression.ExpressionLogicOp(lhs, _, rhs) => DCalErrors.union(analyzeExpression(lhs), analyzeExpression(rhs))
      case Expression.BracketedExpression(expression) => analyzeExpression(expression)
  }

  // For statements that declare and/or define a name, if that name already exists
  private def analyzeStatement(dcalStmt: DCalAST.Statement)(using Context): DCalErrors = {
    dcalStmt match
      case Statement.Await(expression) => analyzeExpression(expression)

      // Loops over list of AssignPair
      // For each, checks that name is (a mutable local or) a state variable and calls analyzeExpression on expression
      case Statement.AssignPairs(assignPairs) => DCalErrors.union(assignPairs.map{ assignPair =>
        val nameErrs = ctx.nameInfoOf.get(assignPair.name) match
          case Some(NameInfo.State) => DCalErrors(Nil)
          case Some(_) => DCalErrors(ReassignmentToImmutable(assignPair.name))
          case None => DCalErrors(NameNotFound(assignPair.name))
        val expressionErrs = analyzeExpression(assignPair.expression)
        DCalErrors.union(nameErrs, expressionErrs)
      })

      case Statement.Let(name, _, expression) =>
        // Check that name has not been declared
        val nameErrs = ctx.nameInfoOf.get(name) match
          case Some(_) => DCalErrors(RedeclaredName(name))
          case None => DCalErrors(Nil)
        val exprErrs = analyzeExpression(expression)
        DCalErrors.union(nameErrs, exprErrs)

      case Statement.Var(name, expressionOpt) =>
        val nameErrs = ctx.nameInfoOf.get(name) match
          case Some(_) => DCalErrors(RedeclaredName(name))
          case None => DCalErrors(Nil)
        val exprErrs = expressionOpt.map {
          case (_, binding) => analyzeExpression(binding)
        }.getOrElse(DCalErrors(Nil))
        DCalErrors.union(nameErrs, exprErrs)

      case Statement.IfThenElse(predicate, thenBlock, elseBlock) =>
        DCalErrors.union(List(analyzeExpression(predicate), analyzeBlock(thenBlock), analyzeBlock(elseBlock)))
  }

  private def analyzeBlock(dcalBlock: DCalAST.Block)(using Context): DCalErrors = {
    def analyzeStatements(stmts: List[DCalAST.Statement], errs: DCalErrors)(using Context): DCalErrors = {
      stmts match
        case Nil => errs
        // Analyzes head, creates a new ctx if necessary, recurses on tail
        case s::ss =>
          val sErrs = analyzeStatement(s)
          s match
            case Statement.Let(name, _, _) => ctx.withNameInfo(name, NameInfo.Local){
              analyzeStatements(ss, DCalErrors.union(errs, sErrs))
            }
            case Statement.Var(name, _) => ctx.withNameInfo(name, NameInfo.State){
              analyzeStatements(ss, DCalErrors.union(errs, sErrs))
            }
            case _ => analyzeStatements(ss, DCalErrors.union(errs, sErrs))
    }
    analyzeStatements(dcalBlock.statements, DCalErrors(Nil))
  }

  private def analyzeDefinition(dcalDef: DCalAST.Definition)(using Context): DCalErrors = {
    // Adds params to ctx before analyzing def body
    val ctxWithParams = dcalDef.params.foldLeft(ctx)( (_ctx, param) =>
      _ctx.withNameInfo(param, NameInfo.Local)
    )
    analyzeBlock(dcalDef.body)(using ctxWithParams)
  }

  private def analyzeImports(dcalModuleName: String, dcalImportNames: List[String]): DCalErrors = {
    // Checks import names do not clash with module name and one another
    val redeclared = (dcalModuleName :: dcalImportNames).groupBy(identity).collect {
      case (redeclaredImportName, _ :: _ :: _) => redeclaredImportName
    }.toList

    // Checks that file matching import name exists in the file system.
    val notFound = dcalImportNames.collect {
      case moduleName if !redeclared.contains(moduleName) && !os.exists(specDir / moduleName) => moduleName
    }

    val circularDependencies = Nil // TODO

    DCalErrors.union(
      List(
        DCalErrors(redeclared.map(RedeclaredName)),
        DCalErrors(notFound.map(ModuleNotFound))
      )
    )
  }

  private def analyzeModule(dcalModule: DCalAST.Module)(using Context): DCalErrors = {
    // Adds module name to context before analyzing imports, so that analyzeImport can check for module name clashes
    val ctxWithModule = ctx.withNameInfo(dcalModule.name, NameInfo.Module)

    val importErrs = analyzeImports(dcalModule.name, dcalModule.imports)
    val ctxWithImports =
        dcalModule.imports.foldLeft(ctxWithModule)((_ctx, importName) =>
          _ctx.withNameInfo(importName, NameInfo.Module)
        )

    // TODO: Parse imported modules & add all their defs to context so that we can scope-check any <name>.<name>.

    // Checks def names do not clash
    // Adds def names to context before analyzing body, because one def can refer to another
    val (ctxWithDefs, defNameErrs) =
      dcalModule.definitions.foldLeft((ctxWithImports, DCalErrors(Nil)))( (acc, aDef) =>
        acc._1.nameInfoOf.get(aDef.name) match
          case None => (
            acc._1.withNameInfo(aDef.name, NameInfo.Definition),
            acc._2
          )
          case Some(_) => (
            acc._1,
            DCalErrors.union(acc._2, DCalErrors(RedeclaredName(aDef.name)))
          )
      )
    val bodyErrs = dcalModule.definitions.map { aDef => analyzeDefinition(aDef)(using ctxWithDefs) }

    // Returns the union of all errors from imports and body
    DCalErrors.union(importErrs::defNameErrs::bodyErrs)
  }

  def apply(contents: String, fileName: String): DCalErrors =
    val dcalModule = DCalParser(contents = contents, fileName = fileName)
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
