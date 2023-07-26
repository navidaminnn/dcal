package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.{Expression, Statement, aCall}

import scala.annotation.tailrec
import scala.util.Either

/**
 * Performs scope analysis on a DCalAST.Module against the following rules:
 * - Names must be declared before they are referenced, except for non-recursive definitions.
 * - Recursive definitions must be declared with `defrec` instead of def`.
 * - A name must not be declared more than once in a scope, even if the redeclaration is done for a different name type.
 * - Imports must be acyclic.
 *
 * There are other rules, for example typechecking rules, reserved for another pass to handle. Namely:
 * - The bindings of all let and var statements must type-check. This means that in a let/var statement, if the
 *   assignment operator is `in`, then the binding must be a set.
 * - Operands in binary, relative, and logical operations must be of the same type.
 */
object DCalScopeAnalyzer {
  enum NameInfo {
    case Local
    case State
    case Definition
    case Module
  }

  inline def ctx(using ctx: Context): Context = ctx

  final case class Context(nameInfoOf: Map[String, NameInfo] = Map.empty, defsOf: Map[List[String], Set[String]] = Map.empty) {
    def withNameInfo(name: String, nameInfo: NameInfo): Context =
      copy(nameInfoOf = nameInfoOf.updated(name, nameInfo))

    def withNameInfo[T](name: String, nameInfo: NameInfo)(fn: Context ?=> T): T =
      fn(using withNameInfo(name = name, nameInfo = nameInfo))

    def withModuleInfo(moduleName: List[String], defNames: Set[String]): Context =
      moduleName match
        case m::Nil => copy(defsOf = defsOf.updated(moduleName, defNames)).withNameInfo(m, NameInfo.Module)
        case _ => copy(defsOf = defsOf.updated(moduleName, defNames))

    def withModuleInfos[T](moduleDefsMap: Map[List[String], Set[String]])(fn: Context ?=> T): T =
      fn(using moduleDefsMap.foldLeft(this) { (_ctx, kv) => _ctx.withModuleInfo(kv._1.tail, kv._2) })
  }

  private val specDir = os.pwd / "src" / "test" / "resources" / "DCal"

  private def findRedeclaredNames(names: List[String])(using Context): Iterable[String] =
    names.groupBy(identity).collect {
      case (redeclaredName, _ :: _ :: _) => redeclaredName
      case (name, _) if ctx.nameInfoOf.contains(name) || ctx.defsOf.contains(List(name)) => name
    }

  private def analyzeCall(call: DCalAST.aCall)(using Context): DCalErrors =
    val moduleDefNameErrs = call match {
      case aCall(Nil, defName, _) =>
        ctx.nameInfoOf.get(defName) match
          case Some(NameInfo.Definition) => DCalErrors(Nil)
          case _ => DCalErrors(DefinitionNotFound(defName))

      case aCall(moduleNames, defName, _) =>
        ctx.defsOf.get(moduleNames) match
          case Some(defs) =>
            if defs(defName) then DCalErrors(Nil) else DCalErrors(MemberNotFound(moduleNames.mkString("."), defName))
          case _ => DCalErrors(ModuleNotFound(moduleNames.mkString(".")))
    }

    val argErrs = call.args.map(analyzeExpression)

    DCalErrors.union(moduleDefNameErrs::argErrs)

  private def analyzeExpression(dcalExpr: DCalAST.Expression)(using Context): DCalErrors = {
    dcalExpr match
      case Expression.True | Expression.False | Expression.IntLiteral(_) | Expression.StringLiteral(_) => DCalErrors(errors = Nil)
      case Expression.Name(name) => ctx.nameInfoOf.get(name) match {
        case Some(_) => DCalErrors(errors = Nil)
        case None => DCalErrors(NameNotFound(name))
      }
      case Expression.Set(members) => DCalErrors.union(members.map(analyzeExpression))
      case Expression.ExpressionUnOp(_, expr) => analyzeExpression(expr)
      case Expression.ExpressionBinOp(lhs, _, rhs) => analyzeExpression(lhs) || analyzeExpression(rhs)
      case Expression.ExpressionRelOp(lhs, _, rhs) => analyzeExpression(lhs) || analyzeExpression(rhs)
      case Expression.ExpressionLogicOp(lhs, _, rhs) => analyzeExpression(lhs) || analyzeExpression(rhs)
      case Expression.BracketedExpression(expression) => analyzeExpression(expression)
  }

  private def analyzeStatement(dcalStmt: DCalAST.Statement)(using Context): DCalErrors = {
    dcalStmt match
      case Statement.Await(expression) => analyzeExpression(expression)

      case Statement.AssignPairs(assignPairs) => DCalErrors.union(assignPairs.map{ assignPair =>
        val nameErrs = ctx.nameInfoOf.get(assignPair.name) match
          case Some(NameInfo.State) => DCalErrors(Nil)
          case Some(_) => DCalErrors(ReassignmentToImmutable(assignPair.name))
          case None => DCalErrors(NameNotFound(assignPair.name))
        val expressionErrs = analyzeExpression(assignPair.expression)
        nameErrs || expressionErrs
      })

      case Statement.Let(name, _, binding) =>
        val nameErrs = ctx.nameInfoOf.get(name) match
          case Some(_) => DCalErrors(RedeclaredName(name))
          case None => DCalErrors(Nil)
        val bindingErrs = binding match
          case Left(call) => analyzeCall(call)
          case Right(expr) => analyzeExpression(expr)
        nameErrs || bindingErrs

      case Statement.Var(name, expressionOpt) =>
        val nameErrs = ctx.nameInfoOf.get(name) match
          case Some(_) => DCalErrors(RedeclaredName(name))
          case None => DCalErrors(Nil)
        val exprErrs = expressionOpt.map {
          case (_, binding) => analyzeExpression(binding)
        }.getOrElse(DCalErrors(Nil))
        nameErrs || exprErrs

      case Statement.IfThenElse(predicate, thenBlock, elseBlock) =>
        analyzeExpression(predicate) || analyzeBlock(thenBlock) || analyzeBlock(elseBlock)

      case Statement.Call(call) => analyzeCall(call)
  }

  private def analyzeBlock(dcalBlock: DCalAST.Block)(using Context): DCalErrors = {
    def analyzeStatements(stmts: List[DCalAST.Statement], errs: DCalErrors)(using Context): DCalErrors = {
      stmts match
        case Nil => errs
        case s::ss =>
          val sErrs = analyzeStatement(s)
          s match
            case Statement.Let(name, _, _) => ctx.withNameInfo(name, NameInfo.Local){
              analyzeStatements(ss, errs || sErrs)
            }
            case Statement.Var(name, _) => ctx.withNameInfo(name, NameInfo.State){
              analyzeStatements(ss, errs || sErrs)
            }
            case _ => analyzeStatements(ss, errs || sErrs)
    }
    analyzeStatements(dcalBlock.statements, DCalErrors(Nil))
  }

  private def analyzeDefinition(dcalDef: DCalAST.Definition)(using Context): DCalErrors = {
    val ctxWithParams = dcalDef.params.foldLeft(ctx)( (_ctx, param) =>
      _ctx.withNameInfo(param, NameInfo.Local)
    )
    analyzeBlock(dcalDef.body)(using ctxWithParams)
  }

  /**
   * Assumes all definitions have been analyzed so that input definitions are free of all scoping errors, except for
   * circular call dependencies. Most importantly, definition names are free of redeclaration and not found errors.
   * TODO: When recursive definition (defrec) is supported, exclude them from this circular definition check.
   */
  private def analyzeDefinitionDependencies(dcalDefs: List[DCalAST.Definition])(using Context): Either[DCalErrors, List[String]] = {
    @tailrec
    def getDependencies(stmts: List[DCalAST.Statement], deps: Iterator[String]): Iterator[String] = {
      stmts match
        case Nil => deps
        case s :: ss => s match
          case DCalAST.Statement.Call(DCalAST.aCall(Nil, defName, _)) =>
            getDependencies(ss, deps ++ Iterator(defName))
          case DCalAST.Statement.Let(_, _, Left(DCalAST.aCall(Nil, defName, _))) =>
            getDependencies(ss, deps ++ Iterator(defName))
          case DCalAST.Statement.IfThenElse(_, thenBlock, elseBlock) =>
            getDependencies(thenBlock.statements ::: elseBlock.statements ::: ss, deps)
          case _ => getDependencies(ss, deps)
    }

    val dependenciesOf: Map[String, List[String]] =
      dcalDefs.foldLeft(Map[String, List[String]]()) { (depsOf, _def) =>
        depsOf + (_def.name -> getDependencies(_def.body.statements, Iterator[String]()).toList)
      }

    Utils.TopologicalSort.sort(dependenciesOf) match {
      case Left(err) => Left(DCalErrors(err))
      case Right(orderedDefs) => Right(orderedDefs)
    }
  }

  private def analyseDefinitions(dcalDefs: List[DCalAST.Definition])(using Context): DCalErrors =
    val ctxWithDefs =
      dcalDefs.foldLeft(ctx)((_ctx, aDef) =>
        _ctx.withNameInfo(aDef.name, NameInfo.Definition)
      )
    val defBodyErrs = DCalErrors.union(dcalDefs.map { aDef => analyzeDefinition(aDef)(using ctxWithDefs) })
    if defBodyErrs.isEmpty then
      analyzeDefinitionDependencies(dcalDefs) match {
        case Left(defDependencyErr) => defDependencyErr
        case Right(_) => DCalErrors(Nil)
      }
    else
      defBodyErrs


  private def analyzeImports(dcalModuleName: String, dcalImportNames: List[String])(using Context): Either[DCalErrors, Map[List[String], Set[String]]] = {
    val redeclared = findRedeclaredNames(dcalImportNames)

    if redeclared.isEmpty then {
      val notFound = dcalImportNames.collect {
        case moduleName if !os.exists(specDir / moduleName) => moduleName
      }

      if notFound.isEmpty then {
        def dfsImport(moduleNames: List[String], importName: String, visited: Set[String], moduleInfosOrErr: Either[CircularImport, Map[List[String], Set[String]]]): Either[CircularImport, Map[List[String], Set[String]]] = {
          if visited(importName) then {
            Left(CircularImport(moduleNames))
          } else {
            moduleInfosOrErr match
              case Left(_) => moduleInfosOrErr
              case Right(_moduleInfos) =>
                val importedModule = DCalParser(contents = os.read(specDir / importName), fileName = importName)
                val newModule = moduleNames :+ importName
                val newDefs = importedModule.definitions.map(_.name).toSet
                val updatedModuleInfos = _moduleInfos + (newModule -> newDefs)
                importedModule.imports.foldLeft(Right(updatedModuleInfos).withLeft[CircularImport]) { (_defs, importedImportName) =>
                  dfsImport(newModule, importedImportName, visited + importName, _defs)
                }
          }
        }

        dcalImportNames.foldLeft(
          Right(Map.empty: Map[List[String], Set[String]]).withLeft[CircularImport]
        ) { (_moduleInfos, importName) =>
          dfsImport(List(dcalModuleName), importName, Set(dcalModuleName), _moduleInfos)
        } match
          case Left(circularErr) => Left(DCalErrors(circularErr))
          case Right(moduleInfos) => Right(moduleInfos)
      } else {
        Left(DCalErrors(notFound.map(ModuleNotFound(_))))
      }
    } else {
      Left(DCalErrors(redeclared.map(RedeclaredName(_)).toList))
    }
  }

  private def analyzeModule(dcalModule: DCalAST.Module)(using Context): DCalErrors = {
    ctx.withNameInfo(dcalModule.name, NameInfo.Module) {
      analyzeImports(dcalModule.name, dcalModule.imports) match {
        case Left(importErrs) => importErrs
        case Right(moduleInfos) =>
          ctx.withModuleInfos(moduleInfos) {
            val redeclaredDefNames = findRedeclaredNames(dcalModule.definitions.map(_.name)).toList
            if redeclaredDefNames.isEmpty then
              analyseDefinitions(dcalModule.definitions)
            else
              DCalErrors(redeclaredDefNames.map(RedeclaredName(_)))
          }
      }
    }
  }

  def apply(contents: String, fileName: String): DCalErrors =
    val dcalModule = DCalParser(contents = contents, fileName = fileName)
    // FIXME: These context entries are for testing. Remove later (possibly when nested def is supported).
    analyzeModule(dcalModule)(using Context(nameInfoOf = Map[String, NameInfo](
      "str" -> NameInfo.State,
      "x" -> NameInfo.State,
      "y" -> NameInfo.State,
      "i" -> NameInfo.State,
      "set" -> NameInfo.State
    )))
}
