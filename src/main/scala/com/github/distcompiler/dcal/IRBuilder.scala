package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.AssignPair
import com.github.distcompiler.dcal.DCalAST.Expression.{BracketedExpression, ExpressionBinOp, IntLiteral, Name, StringLiteral}
import com.github.distcompiler.dcal.DCalAST.Statement.{Await, AssignPairs}
import com.github.distcompiler.dcal.DCalParser.*

/**
 * Compiles DCal concrete syntax to an IR that resembles TLA+. DCal statements that compile to TLA+ identifiers and let
 * expressions are preserved by their respective IR Node structures. All other DCal statements are not interpreted by
 * this IR pass.
 *
 * Here are some examples in the form (DCal, TLA+, IR):
 * a DCal statement, assuming x exists within the domain of some external state structure = (
 *   x := x + 1,
 *   LET _state2 == { [s EXCEPT !.x = s.x + 1]: s \in _state1 } IN <\next statement>,
 *   List(
 *     IR.Node.Let(
 *       name = "_state2",
 *       binding = List(
 *         IR.Node.Uninterpreted("{ ["),
 *         IR.Node.Name("s"), TODO: Ask if we want to preserve the local s, s.x, !.x as names
 *         IR.Node.Uninterpreted("EXCEPT"),
 *         IR.Node.Name("!.x"), TODO: Ask how !.x should be interpreted
 *         IR.Node.Uninterpreted("="),
 *         IR.Node.Name("s.x"),
 *         IR.Node.Uninterpreted("+ 1]:"),
 *         IR.Node.Name("s"),
 *         IR.Node.Uninterpreted("\\in"),
 *         IR.Node.Name("_state1"),
 *         IR.Node.Uninterpreted("}")
 *       ),
 *       body = List(???)
 *     )
 *   )
 * )
 * a DCal statement, assuming p1 and p2 are def parameters = (
 *   let local = p1 + p2,
 *   LET local == p1 + p2 IN <\next statement>,
 *   List(
 *     IR.Node.Let(
 *       name = "local",
 *       binding = List(
 *         IR.Node.Name("p1"),
 *         IR.Node.Uninterpreted("+"),
 *         IR.Node.Name("p2")
 *       ),
 *       body = List(???)
 *     )
 *   )
 * )
 * a DCal def = (
 *  def foo() { x := x + 1 y := y - 1 await x > 2 },
 *  foo(_state1) ==
 *    LET _state2 == { [ s EXCEPT !.x = s.x + 1 ] : s \in _state1 } IN
 *    LET _state3 == { [ s EXCEPT !.y = s.y - 1 ] : s \in _state2 } IN
 *    LET _state4 == { s \in _state3 : s.x > 2 } IN _state4,
 *  List(
 *    IR.Node.Let(
 *      name = "_state2",
 *      binding = List(<\omitted>),
 *      body = List(<\omitted>)
 *    )
 *  )
 * )
 *
 *
 * All DCal statements each compiles to a TLA+ let expression, not another TLA+ statement in a one-to-one
 * mapping manner. The body of the TLA+ let expression depends on the context of the source DCal statement.
 */
object IRBuilder {
  // TODO: go look at TLA+ functions, set, let expressions, operators ()
  // TODO: fresh utility (get a unique name), hash map of names i already used for let bindings -- name cleaner in PGo

  // An TLA+ expression compiles to a List[IR.Node]
  // LET x == 12 IN x \\in {1, y}
  // ||| -->
  //  List(
  //    Node.Let(
  //      name = "x",
  //      binding = List(Node.Uninterpreted("12")),
  //      body = List(
  //        Node.Name("x"),
  //        Node.Uninterpreted("\\in {1, "),
  //        Node.Name("y"),
  //        Node.Uninterpreted("}")
  //      )
  //    )
  //  )

  // 12 \\in {1, y}
  // ||| -->
  //  List(
  //    Node.Uninterpreted("12"),
  //    Node.Uninterpreted("\\in {1, "),
  //    Node.Name("y"),
  //    Node.Uninterpreted("}")
  //  )
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

  // TODO: Ask if each expression compiles to a list of IR nodes, and each statement consists of multiple expression,
  //  how does each statement compiles to a list of IR nodes instead of a list of list of IR nodes? Do we flatten all
  //  list of IR nodes produced by generateExpression?
  def generateStatement(dcalStmt: DCalAST.Statement): List[IR.Node] = {
    def generateAssignPair(dcalAssignPair: DCalAST.AssignPair): List[IR.Node] = List(
      IR.Node.Name(dcalAssignPair.name),
      IR.Node.Uninterpreted("=")
    )

    dcalStmt match {
      case Await(expr) => List(IR.Node.Uninterpreted("await")) ++ generateExpression(expr)
      case AssignPairs(assignPairs) => List(???)
      //      case Await(expression: Expression)
      //      case AssignPairs(assignPairs: List[AssignPair])
      //      case Let(name: String, expression: Expression)
      //      case Var(name: String, opExpression: Option[(BinOp, Expression)])
    }
  }

  def generateDefinition(dcalDef: DCalAST.Definition): IR.Definition = {
    // TODO: Initialize some context objects/tables which track identifier and function declarations
    // TODO: Initialize some builder object which tracks where the code generation is at and where the next generated code goes
    def flatten(dcalStmts: List[List[IR.Node]]): List[IR.Node] = ???

    IR.Definition(
      name = dcalDef.name,
      params = dcalDef.params,
      // TODO: Recursively call generateStatement
      body = flatten(dcalDef.body.statements.map(generateStatement))
    )
  }

  def generateDefinition(dcalImport: String): IR.Definition = ??? // TODO

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
