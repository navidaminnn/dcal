package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.{DCalErrors, DCalParser, DCalScopeAnalyzer, NameNotFound}
import org.scalatest.funsuite.AnyFunSuite

class DCalScopeAnalyzerTest extends AnyFunSuite {
  val testModuleName = "TestModule1"
  val testModule = s"module $testModuleName"

  List(
    // AssignPairs happy path with literal
    s"""$testModule
       |def resetString() { str := "new string" || i := 3; }
       |""".stripMargin -> DCalErrors(Nil),
    // AssignPairs happy path with name rhs
    s"""$testModule
       |def resetString() { i := x; x := y; y := i; }
       |""".stripMargin -> DCalErrors(Nil),
    // AssignPairs lhs not found
    s"""$testModule
       |def resetString() { helloworld := "new string"; }
       |""".stripMargin -> DCalErrors(NameNotFound("helloworld")),
    // AssignPairs rhs not found
    s"""$testModule
       |def resetString() { str := anotherStr; }
       |""".stripMargin -> DCalErrors(NameNotFound("anotherStr")),
    // Reassignment to value
    s"""$testModule
       |def resetString() { let local = "new string"; local := "another new string"; }
       |""".stripMargin -> DCalErrors(ReassignmentToImmutable("local")),
    // Reassignment to param
    s"""$testModule
       |def resetString(p) { p := "new string"; }
       |""".stripMargin -> DCalErrors(ReassignmentToImmutable("p")),
    // Reassignment to def
    s"""$testModule
       |def resetString() { resetString := "new string"; }
       |""".stripMargin -> DCalErrors(ReassignmentToImmutable("resetString")),
    // Reassignment to module
    s"""$testModule
       |def resetString() { $testModuleName := "new string"; }
       |""".stripMargin -> DCalErrors(ReassignmentToImmutable(testModuleName)),
    // Await happy path with literal
    s"""$testModule
       |def testWait() { await x > 4; }
       |""".stripMargin -> DCalErrors(Nil),
    // Await happy path with name rhs
    s"""$testModule
       |def testWait() { await x > y; }
       |""".stripMargin -> DCalErrors(Nil),
    // Await lhs rhs not found
    s"""$testModule
       |def testWait() { await m <= n; }
       |""".stripMargin -> DCalErrors(List(NameNotFound("m"), NameNotFound("n"))),
    // Let happy path
    s"""$testModule
       |def sum(p1, p2) { let local = p1 + p2; x := local; }
       |""".stripMargin -> DCalErrors(Nil),
    // Redeclaration by let
    s"""$testModule
       |def sum(p1, p2) { let y = p1 + p2; x := y; }
       |""".stripMargin -> DCalErrors(RedeclaredName("y")),
    // Let rhs name not found
    s"""$testModule
       |def sum(p) { let local = p1 + p2; x := y + p; }
       |""".stripMargin -> DCalErrors(List(NameNotFound("p1"), NameNotFound("p2"))),
    // Var happy path
    s"""$testModule
       |def testVar(p) { var z = 10; z := p + z; x := x + z; }
       |""".stripMargin -> DCalErrors(Nil),
    // Var redefinition
    s"""$testModule
       |def testVar(p) { var z = 10; var z = 9; x := x + z; }
       |""".stripMargin -> DCalErrors(RedeclaredName("z")),
    // If happy path
    s"""$testModule
       |def testIfThenElseTail() { if x <= y then { i := i - x; x := x + 1; } else { y := y - 1; } }
       |""".stripMargin -> DCalErrors(Nil),
    // If happy path
    s"""$testModule
       |def testIfThenElseNonTail() { if x <= y then { x := x + 1; } else { y := y - 1; } i := x + y; }
       |""".stripMargin -> DCalErrors(Nil),
    // Import happy path
    s"""$testModule
       |import TestModule2, TestModule3, TestModule4
       |""".stripMargin -> DCalErrors(Nil),
    // Import name clash
    s"""$testModule
       |import TestModule2, TestModule2, TestModule3, TestModule4, TestModule3, $testModuleName
       |""".stripMargin
      ->
      DCalErrors(
        List(
          RedeclaredName("TestModule2"),
          RedeclaredName("TestModule3"),
          RedeclaredName(testModuleName)
        )
      ),
    // Circular import
    s"""module TestModule5
       |import TestModule6
       |""".stripMargin -> DCalErrors(CircularDependency("TestModule6", "TestModule5")),
    // Redeclaration of def
    s"""$testModule
       |def resetString() { str := "new string"; }
       |def resetString() {}
       |""".stripMargin -> DCalErrors(RedeclaredName("resetString")),
    // Redeclaration of different name types
    s"""$testModule
       |def $testModuleName() { let $testModuleName = 1; var $testModuleName = $testModuleName; $testModuleName := $testModuleName + 3; }
       |""".stripMargin
      ->
      DCalErrors(
        List(
          RedeclaredName(testModuleName),
          RedeclaredName(testModuleName),
          RedeclaredName(testModuleName)
        )
      )
  ).foreach {
    case (input, expectedOutput) =>
      test(s"DCalScopeAnalyzer($input)") {
        val actualOutput = DCalScopeAnalyzer(contents = input, fileName = "<testfile>")
        assert(actualOutput == expectedOutput)
      }
  }

  List(
    // Place failing tests here
  ).foreach {
    case (input, expectedOutput) =>
      ignore(s"DCalScopeAnalyzer($input)") {
        val actualOutput = DCalScopeAnalyzer(contents = input, fileName = "<testfile>")
        assert(actualOutput == expectedOutput)
      }
  }
}
