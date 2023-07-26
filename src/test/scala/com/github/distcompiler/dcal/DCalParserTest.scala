package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.TestUtils
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable
import scala.collection.immutable.List

class DCalParserTest extends AnyFunSuite {
  import DCalParser.*

  def makeDef(params: List[String], stmts: List[String]): String =
    s"def aFunc(${params.mkString(",")}) { \n${stmts.mkString("\n")} }"

  val testModule = "module TestModule1"
  val testImports = "import TestModule2"

  val testDefNoParamsNoBody = "def aFunc1() {}"
  val expectedDefNoParamsNoBody = DCalAST.Definition(
    name = "aFunc1",
    params = Nil,
    body = DCalAST.Block(statements = Nil)
  )

  val testDefParamsNoBody = "def aFunc2(p1, p2, p3) {}"
  val expectedDefParamsNoBody = DCalAST.Definition(
    name = "aFunc2",
    params = List("p1", "p2", "p3"),
    body = DCalAST.Block(statements = Nil)
  )

  val testLetEqualToLiteral = "let test1 = TRUE;"
  val testVar = "var test2;"
  val testVarEquals = """var test3 = "val3";"""
  val testVarSlashIn = "var test4 \\in { 1, 2, 3, 4, 5 };"
  val testIf = "if x <= y then { x := x + 1; } else { y := y - 1; }"
  val testCall = """aFunc2(1, 2 + 3, "val");"""
  val testImportedDefCall = "TestModule2.aFunc3();"
  val testChainedImportedDefCall = "TestModule2.TestModule3.TestModule4.aFunc5();"
  val testBracketedExpression = "(test6)"
  val testAssignPairs = s"test6 := test7 || test7 := $testBracketedExpression;"
  val testExpression = "test6 > 1000"
  val testAwait = s"await $testExpression;"
  val testLetEqualToCall = "let test8 = aFunc4();"
  val testDefParamsBody = makeDef(
    List("anArg"),
    List(
      testLetEqualToLiteral,
      testLetEqualToCall,
      testVar,
      testVarEquals,
      testVarSlashIn,
      testAssignPairs,
      testAwait,
      testIf,
      testCall,
      testImportedDefCall,
      testChainedImportedDefCall
    )
  )

  List(
    testModule -> DCalAST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = Nil
    ),
    TestUtils.sequenceLines(testModule, testImports) -> DCalAST.Module(
      name = "TestModule1",
      imports = List("TestModule2"),
      definitions = Nil
    ),
    TestUtils.sequenceLines(testModule, testDefNoParamsNoBody) -> DCalAST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = List(
        expectedDefNoParamsNoBody
      )
    ),
    TestUtils.sequenceLines(testModule, testDefParamsBody) -> DCalAST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = List(
        DCalAST.Definition(
          name = "aFunc",
          params = List("anArg"),
          body = DCalAST.Block(
            statements = List(
              DCalAST.Statement.Let(
                name = "test1",
                assignmentOp = DCalAST.AssignmentOp.EqualTo,
                binding = Right(DCalAST.Expression.True)
              ),
              DCalAST.Statement.Let(
                name = "test8",
                assignmentOp = DCalAST.AssignmentOp.EqualTo,
                binding = Left(DCalAST.aCall(moduleName = Nil, definitionName = "aFunc4", args = Nil))
              ),
              DCalAST.Statement.Var(name = "test2", expressionOpt = None),
              DCalAST.Statement.Var(
                name = "test3",
                expressionOpt = Some((DCalAST.AssignmentOp.EqualTo, DCalAST.Expression.StringLiteral("val3")))
              ),
              DCalAST.Statement.Var(
                name = "test4",
                expressionOpt = Some(
                  (
                    DCalAST.AssignmentOp.SlashIn,
                    DCalAST.Expression.Set(
                      members = List(
                        DCalAST.Expression.IntLiteral(1),
                        DCalAST.Expression.IntLiteral(2),
                        DCalAST.Expression.IntLiteral(3),
                        DCalAST.Expression.IntLiteral(4),
                        DCalAST.Expression.IntLiteral(5),
                      )
                    )
                  )
                )),
              DCalAST.Statement.AssignPairs(
                assignPairs = List(
                  DCalAST.AssignPair(name = "test6", expression = DCalAST.Expression.Name("test7")),
                  DCalAST.AssignPair(name = "test7", expression = DCalAST.Expression.Name("test6"))
                )
              ),
              DCalAST.Statement.Await(
                expression = DCalAST.Expression.ExpressionRelOp(
                  lhs = DCalAST.Expression.Name("test6"),
                  relOp = DCalAST.RelOp.GreaterThan,
                  rhs = DCalAST.Expression.IntLiteral(1000)
                )
              ),
              DCalAST.Statement.IfThenElse(
                predicate = DCalAST.Expression.ExpressionRelOp(
                  lhs = DCalAST.Expression.Name("x"),
                  relOp = DCalAST.RelOp.LesserThanOrEqualTo,
                  rhs = DCalAST.Expression.Name("y")
                ),
                thenBlock = DCalAST.Block(
                  statements = List(
                    DCalAST.Statement.AssignPairs(
                      assignPairs = List(
                        DCalAST.AssignPair(
                          name = "x",
                          expression = DCalAST.Expression.ExpressionBinOp(
                            lhs = DCalAST.Expression.Name("x"),
                            binOp = DCalAST.BinOp.Plus,
                            rhs = DCalAST.Expression.IntLiteral(1)
                          )
                        )
                      )
                    )
                  )
                ),
                elseBlock = DCalAST.Block(
                  statements = List(
                    DCalAST.Statement.AssignPairs(
                      assignPairs = List(
                        DCalAST.AssignPair(
                          name = "y",
                          expression = DCalAST.Expression.ExpressionBinOp(
                            lhs = DCalAST.Expression.Name("y"),
                            binOp = DCalAST.BinOp.Minus,
                            rhs = DCalAST.Expression.IntLiteral(1)
                          )
                        )
                      )
                    )
                  )
                )
              ),
              DCalAST.Statement.Call(
                call = DCalAST.aCall(
                  moduleName = Nil,
                  definitionName = "aFunc2",
                  args = List(
                    DCalAST.Expression.IntLiteral(1),
                    DCalAST.Expression.ExpressionBinOp(
                      lhs = DCalAST.Expression.IntLiteral(2),
                      binOp = DCalAST.BinOp.Plus,
                      rhs = DCalAST.Expression.IntLiteral(3)
                    ),
                    DCalAST.Expression.StringLiteral("val")
                  )
                )
              ),
              DCalAST.Statement.Call(
                call = DCalAST.aCall(
                  moduleName = List("TestModule2"),
                  definitionName = "aFunc3",
                  args = Nil
                )
              ),
              DCalAST.Statement.Call(
                call = DCalAST.aCall(
                  moduleName = List("TestModule2", "TestModule3", "TestModule4"),
                  definitionName = "aFunc5",
                  args = Nil
                )
              )
            )
          )
        )
      )
    ),
    TestUtils.sequenceLines(testModule, testDefNoParamsNoBody, testDefParamsNoBody) -> DCalAST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = List(expectedDefNoParamsNoBody, expectedDefParamsNoBody)
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

  List(
    "BasicModule" -> DCalAST.Module(
      name = "TestModule1",
      imports = List("TestModule2"),
      definitions = List(
        DCalAST.Definition(
          name = "aFunc1", params = Nil, body = DCalAST.Block(statements = Nil)
        ),
        DCalAST.Definition(
          name = "aFunc2", params = List("p1", "p2", "p3"), body = DCalAST.Block(statements = Nil)
        ),
        DCalAST.Definition(
          name = "aFunc3",
          params = Nil,
          body = DCalAST.Block(
            statements = List(
              DCalAST.Statement.AssignPairs(
                assignPairs = List(
                  DCalAST.AssignPair(
                    name = "x",
                    expression = DCalAST.Expression.ExpressionBinOp(
                      lhs = DCalAST.Expression.Name("x"),
                      binOp = DCalAST.BinOp.Plus,
                      rhs = DCalAST.Expression.IntLiteral(1)
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ).foreach {
    case (input, expectedResult) =>
      test(s"parse file $input") {
        val actualResult = DCalParser(
          contents = TestUtils.readTestFile(input), fileName = input
        )
        assert(actualResult == expectedResult)
      }
  }
}
