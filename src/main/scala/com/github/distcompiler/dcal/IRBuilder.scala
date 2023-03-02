package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.Expression.{BracketedExpression, ExpressionBinOp, IntLiteral, Name, StringLiteral}
import com.github.distcompiler.dcal.DCalAST.Statement.{AssignPairs, Await, Let, Var, If}
import com.github.distcompiler.dcal.DCalParser.*

/**
 * Compiles DCal concrete syntax to an IR that resembles TLA+. DCal statements that compile to TLA+ identifiers, let
 * expressions, and set maps are preserved by their respective IR Node structures. All other DCal statements are
 * uninterpreted by this IR pass.
 *
 * Each DCal statement compiles to a TLA+ nested let expression, not another TLA+ statement in a one-to-one mapping
 * manner.
 */
object IRBuilder {
  // TODO: fresh utility (get a unique name)

  def generateExpression(dcalExpr: DCalAST.Expression): List[IR.Node] = {
    dcalExpr match {
      case ExpressionBinOp(lhs, binOp, rhs) =>
        generateExpression(lhs) ++ List(IR.Node.Uninterpreted(binOp.toString)) ++ generateExpression(rhs)

      case IntLiteral(value) => List(IR.Node.Uninterpreted(value.toString))

      case StringLiteral(value) => List(IR.Node.Uninterpreted(value))

      case Name(name) => List(IR.Node.Name(name = name))

      case BracketedExpression(expr) =>
        List(IR.Node.Uninterpreted("(")) ++ generateExpression(expr) ++ List(IR.Node.Uninterpreted(")"))
    }
  }

  def generateStatement(dcalStmt: DCalAST.Statement): List[IR.Node] = {
    dcalStmt match {
      case Await(expr) => List(IR.Node.Uninterpreted("await")) ++ generateExpression(expr)
      case AssignPairs(assignPairs) => List(???)
      case Let(name: String, expression: DCalAST.Expression) => ???
      case Var(name: String, opExpression: Option[(DCalAST.BinOp, DCalAST.Expression)]) => ???
      case If(predicate: DCalAST.Expression, thenBlock: DCalAST.Block, elseBlock: Option[DCalAST.Block]) => ???
    }
  }

  def generateDefinition(dcalDef: DCalAST.Definition): IR.Definition = {
    IR.Definition(
      name = dcalDef.name,
      params = dcalDef.params,
      // TODO: Recursively call generateStatement because in a sequence of statements, the (i+1)th statement needs to
      //  be stitched up as the body of the ith statement let expression
      body = ??? // dcalDef.body.statements.map(generateStatement)
    )
  }

  def generateDefinition(dcalImport: String): IR.Definition = ???

  def build(dcalModule: DCalAST.Module): IR.Module = {
    // Construct the IR Module to return, which holds all the generated TLA+ code
    val definitions = dcalModule.definitions.map(generateDefinition)
    val imports = dcalModule.imports.map(generateDefinition)
    IR.Module(
      name = dcalModule.name,
      definitions = imports ++ definitions,
    )
  }

  def apply(contents: String, fileName: String): IR.Module = {
    // Set up logging
    // Parse DCal contents
    val dcalModule = DCalParser(contents = contents, fileName = fileName)
    // Build the IR
    build(dcalModule = dcalModule)
    // Log the IR
  }
}
