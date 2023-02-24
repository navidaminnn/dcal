package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable
import scala.collection.immutable.List

class TestDCalParser extends AnyFunSuite {
  import DCalParser.*

  def sequenceLines(lines: String*): String = lines.mkString("\n")

  val testModule = "module TestModule1"
  val testImports = "import TestModule2"

  val testDefNoParamsNoBody1 = "def aFunc1 () {}"
  val expectedDefNoParamsNoBody1 = AST.Definition(
    name = "aFunc1",
    params = Nil,
    body = AST.Block(statements = Nil)
  )

  val testDefNoParamsNoBody2 = "def aFunc2 (p1, p2, p3) {}"
  val expectedDefNoParamsNoBody2 = AST.Definition(
    name = "aFunc2",
    params = List("p1", "p2", "p3"),
    body = AST.Block(statements = Nil)
  )

  val testLet = "let test1 = 1499"
  val testVar = "var test2"
  val testVarEquals = """var test3 = "val3""""
  val testVarSlashIn = "var test4 \\in test5"
  val testBracketedExpression = "(test6)"
  val testAssignPairs = s"test6 := test7 || test7 := ${testBracketedExpression}"
  val testExpression = "test6 BinOpPlaceholder 1000"
  val testAwait = s"await ${testExpression}"
  val testDefParamsBody = s"def aFunc (anArg) {\n${
    sequenceLines(
      testLet, testVar, testVarEquals, testVarSlashIn, testAssignPairs, testAwait)
  }\n}"

  List(
    testModule -> AST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = Nil
    ),
    sequenceLines(testModule, testImports) -> AST.Module(
      name = "TestModule1",
      imports = List("TestModule2"),
      definitions = Nil
    ),
    sequenceLines(testModule, testDefNoParamsNoBody1) -> AST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = List(
        expectedDefNoParamsNoBody1
      )
    ),
    sequenceLines(testModule, testDefParamsBody) -> AST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = List(
        AST.Definition(
          name = "aFunc",
          params = List("anArg"),
          body = AST.Block(statements = List(
            AST.Statement.Let(name = "test1", expression = AST.Expression.IntLiteral(1499)),
            AST.Statement.Var(name = "test2", opExpression = None),
            AST.Statement.Var(
              name = "test3",
              opExpression = Some((AST.BinOp.Equals, AST.Expression.StringLiteral("val3")))
            ),
            AST.Statement.Var(
              name = "test4",
              opExpression = Some((AST.BinOp.SlashIn, AST.Expression.Name("test5")))),
            AST.Statement.AssignPairs(
              assignPairs = List(
                AST.AssignPair(name = "test6", expression = AST.Expression.Name("test7")),
                AST.AssignPair(name = "test7", expression = AST.Expression.Name("test6"))
              )
            ),
            AST.Statement.Await(expression = AST.Expression.ExpressionBinOp(
              lhs = AST.Expression.Name("test6"),
              binOp = AST.BinOp.Placeholder,
              rhs = AST.Expression.IntLiteral(1000)))
          ))
        )
      )
    ),
    sequenceLines(testModule, testDefNoParamsNoBody1, testDefNoParamsNoBody2) -> AST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = List(expectedDefNoParamsNoBody1, expectedDefNoParamsNoBody2)
    )
  ).foreach {
    case (input, expectedResult) =>
      test(s"parse($input)") {
        val actualResult = DCalParser(
          contents = input,
          fileName = "<testfile>",
        )
        assert(actualResult == expectedResult)
      }
  }
}
