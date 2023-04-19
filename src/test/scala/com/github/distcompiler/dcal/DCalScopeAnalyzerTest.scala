package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.{DCalErrors, DCalParser, DCalScopeAnalyzer, NameNotFound}
import org.scalatest.funsuite.AnyFunSuite

class DCalScopeAnalyzerTest extends AnyFunSuite {
  final case class TestCase(description: String, dcalModule: String, expectedScopeErrs: DCalErrors)

  val testModuleName = "TestModule1"
  val testModule = s"module $testModuleName"

  List(
    TestCase(
      description = "AssignPairs, happy path w/ literal rhs",
      dcalModule =
        s"""$testModule
           |def resetString() { str := "new string" || i := 3; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "AssignPairs, happy path with name rhs",
      dcalModule =
        s"""$testModule
           |def resetString() { i := x; x := y; y := i; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "AssignPairs, lhs not found",
      dcalModule =
        s"""$testModule
           |def resetString() { helloworld := "new string"; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(NameNotFound("helloworld"))
    ),
    TestCase(
      description = "AssignPairs, rhs not found",
      dcalModule =
        s"""$testModule
           |def resetString() { str := anotherStr; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(NameNotFound("anotherStr"))
    ),
    TestCase(
      description = "AssignPairs, reassignment to value",
      dcalModule =
        s"""$testModule
           |def resetString() { let local = "new string"; local := "another new string"; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(ReassignmentToImmutable("local"))
    ),
    TestCase(
      description = "AssignPairs, reassignment to param",
      dcalModule =
        s"""$testModule
           |def resetString(p) { p := "new string"; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(ReassignmentToImmutable("p"))
    ),
    TestCase(
      description = "AssignPairs, reassignment to def",
      dcalModule =
        s"""$testModule
           |def resetString() { resetString := "new string"; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(ReassignmentToImmutable("resetString"))
    ),
    TestCase(
      description = "AssignPairs, reassignment to module",
      dcalModule =
        s"""$testModule
           |def resetString() { $testModuleName := "new string"; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(ReassignmentToImmutable(testModuleName))
    ),
    TestCase(
      description = "AssignPairs, reassignment to shadowing param",
      dcalModule =
        s"""$testModule
           |def mt(x, y) { x := x + y; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(ReassignmentToImmutable("x"))
    ),
    TestCase(
      description = "Await, happy path w/ literal",
      dcalModule =
        s"""$testModule
           |def testWait() { await x > 4; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Await, happy path w/ name",
      dcalModule =
        s"""$testModule
           |def testWait() { await x > y; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Await, name not found",
      dcalModule =
        s"""$testModule
           |def testWait() { await m <= n; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(List(NameNotFound("m"), NameNotFound("n")))
    ),
    TestCase(
      description = "Let, expr binding happy path",
      dcalModule =
        s"""$testModule
           |def sum(p1, p2) { let local = p1 + p2; x := local; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Let, procedure call binding happy path",
      dcalModule =
        s"""$testModule
           |def mt() {}
           |def sum(p1, p2) { let local = mt(); x := local; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Let, procedure call binding not found",
      dcalModule =
        s"""$testModule
           |def func1() { let local = func2(); x := local; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(DefinitionNotFound("func2"))
    ),
    TestCase(
      description = "Let, redeclaration",
      dcalModule =
        s"""$testModule
           |def sum(p1, p2) { let y = p1 + p2; x := y; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(RedeclaredName("y"))
    ),
    TestCase(
      description = "Let, rhs name not found",
      dcalModule =
        s"""$testModule
           |def sum(p) { let local = p1 + p2; x := y + p; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(List(NameNotFound("p1"), NameNotFound("p2")))
    ),
    TestCase(
      description = "Var, happy path",
      dcalModule =
        s"""$testModule
           |def testVar(p) { var z = 10; z := p + z; x := x + z; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Var, redeclaration",
      dcalModule =
        s"""$testModule
           |def testVar(p) { var z = 10; var z = 9; x := x + z; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(RedeclaredName("z"))
    ),
    TestCase(
      description = "If, happy path 1",
      dcalModule =
        s"""$testModule
           |def testIfThenElseTail() { if x <= y then { i := i - x; x := x + 1; } else { y := y - 1; } }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "If, happy path 2",
      dcalModule =
        s"""$testModule
           |def testIfThenElseNonTail() { if x <= y then { x := x + 1; } else { y := y - 1; } i := x + y; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "If, then & else must be of different scopes, happy path",
      dcalModule =
        s"""$testModule
           |def testThenElseScopes() { if x # y then { var l = x + y; i := i + l; } else { let l = x + y; i := i - l; } }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "If, then & else must be of different scopes, err path",
      dcalModule =
        s"""$testModule
           |def testThenElseScopes() { if x # y then { var l = x + y; i := i + l; } else { i := i - l; } }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(NameNotFound("l"))
    ),
    TestCase(
      description = "Procedure call, args not found",
      dcalModule =
        s"""$testModule
           |def foo(p1, p2, p3, p4, p5) {}
           |def bar() { foo(1, 2, 3, v, 5); }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(NameNotFound("v"))
    ),
    TestCase(
      description = "Procedure call, recursive",
      dcalModule =
        s"""$testModule
           |def func1() { if x = 0 then {} else { func1(); } }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(CircularDefinition("func1"))
    ),
    TestCase(
      description = "Procedure call, mutually recursive",
      dcalModule =
        s"""$testModule
           |def func1() { if x = 0 then {} else { func2(); } }
           |def func2() { x := x - 1; func1(); }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(CircularDefinition("func1"))
    ),
    TestCase(
      description = "Import, happy path",
      dcalModule =
        s"""$testModule
           |import TestModule2, TestModule3, TestModule4
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Import, not found",
      dcalModule =
        s"""$testModule
           |import TestModule10
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(ModuleNotFound("TestModule10"))
    ),
    TestCase(
      description = "Import, redeclaration",
      dcalModule =
        s"""$testModule
           |import TestModule2, TestModule2, TestModule3, TestModule4, TestModule3, $testModuleName
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(
        List(
          RedeclaredName("TestModule2"),
          RedeclaredName("TestModule3"),
          RedeclaredName(testModuleName)
        )
      )
    ),
    TestCase(
      description = "Imported procedure call, happy path",
      dcalModule =
        s"""$testModule
           |import TestModule2
           |def foo(p1, p2) { let local = TestModule2.subtract(p1, p2); x := local; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Imported procedure call, module not found",
      dcalModule =
        s"""$testModule
           |def foo() { TestModule2.subtract(7, 3); }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(ModuleNotFound("TestModule2"))
    ),
    TestCase(
      description = "Imported procedure call, same def name in different modules",
      dcalModule =
        s"""$testModule
           |import TestModule2
           |def subtract(p1, p2) { let local = TestModule2.subtract(p1, p2); x := local; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Imported procedure call, procedure not a member of imported module",
      dcalModule =
        s"""$testModule
           |import TestModule2
           |def add(p1, p2) {}
           |def foo(p1, p2) { TestModule2.add(p1, p2); }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(MemberNotFound("TestModule2", "add"))
    ),
    TestCase(
      description = "Chained imported procedure call, happy path",
      dcalModule =
        s"""$testModule
           |import TestModule2
           |def foo() { TestModule2.TestModule3.foo(); }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Chained imported procedure call, def not found",
      dcalModule =
        s"""$testModule
           |import TestModule2
           |def foo() { TestModule2.TestModule3.bar(); }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(MemberNotFound("TestModule2.TestModule3", "bar"))
    ),
    TestCase(
      description = "Chained imported procedure call, module not found",
      dcalModule =
        s"""$testModule
           |import TestModule2
           |def foo() { TestModule2.TestModule4.foo(); }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(ModuleNotFound("TestModule2.TestModule4"))
    ),
    TestCase(
      description = "Def not found",
      dcalModule =
        s"""$testModule
           |def func1() { func2(); }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(DefinitionNotFound("func2"))
    ),
    TestCase(
      description = "Def, redeclaration",
      dcalModule =
        s"""$testModule
           |def resetString() { str := "new string"; }
           |def resetString() {}
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(RedeclaredName("resetString"))
    ),
    TestCase(
      description = "Def, params shadowing locals / state variables",
      dcalModule =
        s"""$testModule
           |def mt(x, y) { i := x + y; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(Nil)
    ),
    TestCase(
      description = "Def redeclares module name",
      dcalModule =
        s"""$testModule
           |def $testModuleName() {}
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(List(RedeclaredName(testModuleName)))
    ),
    TestCase(
      description = "Def redeclares import name",
      dcalModule =
        s"""$testModule
           |import TestModule2
           |def TestModule2() {}
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(List(RedeclaredName("TestModule2")))
    ),
    TestCase(
      description = "Let redeclares module name",
      dcalModule =
        s"""$testModule
           |def foo() { let $testModuleName = 1; i := $testModuleName; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(List(RedeclaredName(testModuleName)))
    ),
    TestCase(
      description = "Let redeclares import name",
      dcalModule =
        s"""$testModule
           |import TestModule2
           |def foo() { let TestModule2 = 1; i := TestModule2; }
           |""".stripMargin,
      expectedScopeErrs = DCalErrors(List(RedeclaredName("TestModule2")))
    )
  ).foreach {
    case TestCase(description, dcalModule, expectedScopeErrs) =>
      test(description) {
        val actualOutput = DCalScopeAnalyzer(contents = dcalModule, fileName = "<testfile>")
        assert(actualOutput == expectedScopeErrs)
      }
  }

  List(
    // Place tests that read from the file system here
    // Circular import
    "TestModule5" -> DCalErrors(CircularImport(List("TestModule5", "TestModule6"))),
    // Indirect circular import
    "TestModule7".stripMargin -> DCalErrors(CircularImport(List("TestModule7", "TestModule8", "TestModule9"))),
  ).foreach {
    case (input, expectedOutput) =>
      test(s"analyze scope file $input") {
        val actualOutput = DCalScopeAnalyzer(contents = TestUtils.readTestFile(input), fileName = input)
        assert(actualOutput == expectedOutput)
      }
  }
}
