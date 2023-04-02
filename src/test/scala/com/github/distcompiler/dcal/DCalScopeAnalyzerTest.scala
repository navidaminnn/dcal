package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.{DCalErrors, DCalParser, DCalScopeAnalyzer, NameNotFound}
import org.scalatest.funsuite.AnyFunSuite

class DCalScopeAnalyzerTest extends AnyFunSuite {
  val moduleName = "TestModule"
  val testModule = s"module $moduleName"

  List(
    // AssignPairs happy path with literal
    s"""$testModule
      |def resetString() { str := "new string" || i := 3; }
      |""".stripMargin -> None,
    // AssignPairs happy path with name rhs
    s"""$testModule
      |def resetString() { i := x; x := y; y := i; }
      |""".stripMargin -> None,
    // Redefinition of def
    s"""$testModule
      |def resetString() { str := "new string"; }
      |def resetString() {}
      |""".stripMargin -> Some(DCalErrors(RedeclaredName("resetString"))),
    // AssignPairs lhs not found
    s"""$testModule
      |def resetString() { helloworld := "new string"; }
      |""".stripMargin -> Some(DCalErrors(NameNotFound("helloworld"))),
    // Reassignment to param
    s"""$testModule
      |def resetString(p) { p := "new string"; }
      |""".stripMargin -> Some(DCalErrors(ReassignmentToImmutable("p"))),
    // AssignPairs rhs not found
    s"""$testModule
      |def resetString() { str := anotherStr; }
      |""".stripMargin -> Some(DCalErrors(NameNotFound("anotherStr"))),
    // Await happy path with literal
    s"""$testModule
      |def testWait() { await x > 4; }
      |""".stripMargin -> None,
    // Await happy path with name rhs
    s"""$testModule
      |def testWait() { await x > y; }
      |""".stripMargin -> None,
    // Await lhs rhs not found
    s"""$testModule
       |def testWait() { await m <= n; }
       |""".stripMargin -> Some(DCalErrors(List(NameNotFound("m"), NameNotFound("n")))),
    // Let happy path
    s"""$testModule
      |def sum(p1, p2) { let local = p1 + p2; x := local; }
      |""".stripMargin -> None,
    // Let redefinition
    s"""$testModule
       |def sum(p1, p2) { let y = p1 + p2; x := y; }
       |""".stripMargin -> Some(DCalErrors(RedeclaredName("y"))),
    // Let rhs name not found
    s"""$testModule
       |def sum(p) { let local = p1 + p2; x := y + p; }
       |""".stripMargin -> Some(DCalErrors(List(NameNotFound("p1"), NameNotFound("p2")))),
    // Var happy path
    s"""$testModule
      |def testVar(p) { var z = 10; z := p + z; x := x + z; }
      |""".stripMargin -> None,
    // Var redefinition
    s"""$testModule
       |def testVar(p) { var z = 10; var z = 9; x := x + z; }
       |""".stripMargin -> Some(DCalErrors(RedeclaredName("z"))),
    // If happy path
    s"""$testModule
      |def testIfThenElseTail() { if x <= y then { i := i * x; x := x + 1; } else { y := y - 1; } }
      |""".stripMargin -> None,
    // If happy path
    s"""$testModule
       |def testIfThenElseNonTail() { if x <= y then { x := x + 1; } else { y := y - 1; } i := x + y; }
       |""".stripMargin -> None,
  ).foreach {
    case (input, expectedOutput) =>
      test(s"DCalScopeAnalyzer(DCalParser($input))") {
        val parsed = DCalParser(
          contents = input,
          fileName = "<testfile>",
        )
        val actualOutput = DCalScopeAnalyzer(dcalModule = parsed)
        assert(actualOutput == expectedOutput)
      }
  }
}
