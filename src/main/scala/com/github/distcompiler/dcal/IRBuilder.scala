package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.{Expression, Statement}
import com.github.distcompiler.dcal.DCalAST.Expression.*
import com.github.distcompiler.dcal.DCalAST.Statement.*
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
    dcalExpr match
      case Expression.True => ???
      case Expression.False => ???
      case Expression.IntLiteral(value) => List(IR.Node.Uninterpreted(value.toString))
      case Expression.StringLiteral(value) => List(IR.Node.Uninterpreted(value))
      case Expression.Name(name) => List(IR.Node.Name(name = name))
      case Expression.Set(members) => ???
      case Expression.ExpressionBinOp(lhs, binOp, rhs) =>
        generateExpression(lhs) ++ List(IR.Node.Uninterpreted(binOp.toString)) ++ generateExpression(rhs)
      case Expression.ExpressionRelOp(lhs, relOp, rhs) => ???
      case Expression.ExpressionLogicOp(lhs, logicOp, rhs) => ???
      case Expression.ExpressionUnOp(unop, expr) => ???
      case Expression.BracketedExpression(expr) =>
        List(IR.Node.Uninterpreted("(")) ++ generateExpression(expr) ++ List(IR.Node.Uninterpreted(")"))
  }

  def generateStatement(dcalStmt: DCalAST.Statement): List[IR.Node] = {
    dcalStmt match
      case Statement.Await(expr) => List(IR.Node.Uninterpreted("await")) ++ generateExpression(expr)
      case Statement.AssignPairs(assignPairs) => ???
      case Statement.Let(name, assignmentOp, expression) => ???
      case Statement.Var(name, expressionOpt) => ???
      case Statement.IfThenElse(predicate, thenBlock, elseBlock) => ???
      case Statement.Call(call) => ???
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
