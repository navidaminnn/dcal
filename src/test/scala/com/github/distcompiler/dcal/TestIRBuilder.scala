package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class TestIRBuilder extends AnyFunSuite {
  // TODO: Ask if there is any case where a def body translates to more than a TLA+ let expression -> last TLA+ body
  val moduleName = "TestModule"
  val testModule = s"module $moduleName"

  val testDefNoParamsNoBody = "def mt() {}"
  val expectedDefNoParamsNoBody = IR.Definition(
    name = "mt",
    params = List("_state1"),
    body = List(
      IR.Node.Name("_state1")
    )
  )
  
  val testDefWithAssignment = """def resetString() { str = "new string" }"""
  // Expected TLA+:
  //  resetString(_state1) ==
  //    LET
  //      _state2 == { [s EXCEPT !.str = "new string"]: s \in _state1 }
  //    IN
  //      _state2
  val expectedDefWithAssignment = IR.Definition(
    name = "resetString",
    params = List("_state1"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        binding = List(
          IR.Node.Uninterpreted("{ ["),
          IR.Node.Name("s"),
          IR.Node.Uninterpreted("EXCEPT"),
          IR.Node.Name("!.str"),
          IR.Node.Uninterpreted("""= "new string"]:"""),
          IR.Node.Name("s"),
          IR.Node.Uninterpreted("\\in"),
          IR.Node.Name("_state1"),
          IR.Node.Uninterpreted("}")
        ),
        body = List(
          IR.Node.Name("_state2")
        )
      )
    )
  )

  val testDefWithLet = s"def sum(p1, p2) { let local = p1 + p2 x := local }"
  // Expected TLA+:
  //  sum(_state1, p1, p2) ==
  //    LET
  //      local == p1 + p2
  //    IN
  //      LET
  //        _state2 == { [s EXCEPT !.x = local]: s \ in _state1 }
  //      IN
  //        _state2
  val expectedDefWithLet = IR.Definition(
    name = "sum",
    params = List("_state1", "p1", "p2"),
    body = List(
      IR.Node.Let(
        name = "local",
        binding = List(
          IR.Node.Name("p1"),
          IR.Node.Uninterpreted("+"),
          IR.Node.Name("p2")
        ),
        body = List(
          IR.Node.Let(
            name = "_state2",
            // { [s EXCEPT !.x = local]: s \ in _state1 }
            binding = List(
              IR.Node.Uninterpreted("{ ["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted("EXCEPT"),
              IR.Node.Name("!.x"),
              IR.Node.Uninterpreted("="),
              IR.Node.Name("local"),
              IR.Node.Uninterpreted("]:"),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted("\\in"),
              IR.Node.Name("_sate1"),
              IR.Node.Uninterpreted("}")
            ),
            body = List(
              IR.Node.Name("_state2")
            )
          )
        )
      )
    )
  )

  val testMultiLineDef = s"def change(v) { y := y - v i := i + 1 }"
  // Expected TLA+:
  //  change(_state1, v) ==
  //    LET
  //      _state2 == { [s EXCEPT !.y = s.y - v]: s \ in _state1 }
  //    IN
  //      LET
  //        _state3 == { [s EXCEPT !.i = s.i + 1]: s \ in _state2 }
  //      IN
  //        _state3
  val expectedMultiLineDef = IR.Definition(
    name = "change",
    params = List("_state1", "v"),
    body = List(
      IR.Node.Let(
        name = "_state2",
        // { [s EXCEPT !.y = s.y - v]: s \ in _state1 }
        binding = List(
          IR.Node.Uninterpreted("{ ["),
          IR.Node.Name("s"),
          IR.Node.Uninterpreted("EXCEPT"),
          IR.Node.Name("!.y"),
          IR.Node.Uninterpreted("="),
          IR.Node.Name("s.y"),
          IR.Node.Uninterpreted("-"),
          IR.Node.Name("v"),
          IR.Node.Uninterpreted("]:"),
          IR.Node.Name("s"),
          IR.Node.Uninterpreted("\\in"),
          IR.Node.Name("_state1"),
          IR.Node.Uninterpreted("}")
        ),
        body = List(
          IR.Node.Let(
            name = "_state3",
            // { [s EXCEPT !.i = s.i + 1]: s \in _state2 }
            binding = List(
              IR.Node.Uninterpreted("{ ["),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted("EXCEPT"),
              IR.Node.Name("!.i"),
              IR.Node.Uninterpreted("="),
              IR.Node.Name("s.i"),
              IR.Node.Uninterpreted("+ 1]:"),
              IR.Node.Name("s"),
              IR.Node.Uninterpreted("\\in"),
              IR.Node.Name("_state2"),
              IR.Node.Uninterpreted("}")
            ),
            body = List(
              IR.Node.Name("_state3")
            )
          )
        )
      )
    )
  )

  List(
    testModule -> IR.Module(
      name = moduleName, definitions = Nil
    ),
    TestUtils.sequenceLines(testModule, testDefNoParamsNoBody) -> IR.Module(
      name = moduleName,
      definitions = List(
        expectedDefNoParamsNoBody
      )
    ),
    TestUtils.sequenceLines(testModule, testDefWithAssignment) -> IR.Module(
      name = moduleName,
      definitions = List(
        expectedDefWithAssignment
      )
    ),
    TestUtils.sequenceLines(testModule, testDefWithLet) -> IR.Module(
      name = moduleName,
      definitions = List(
        expectedDefWithLet
      )
    ),
    TestUtils.sequenceLines(testModule, testMultiLineDef) -> IR.Module(
      name = moduleName,
      definitions = List(
        expectedMultiLineDef
      )
    ),
    TestUtils.sequenceLines(testModule, testDefWithAssignment, testMultiLineDef, testDefNoParamsNoBody) -> IR.Module(
      name = moduleName,
      definitions = List(
        expectedDefWithAssignment,
        expectedMultiLineDef,
        expectedDefNoParamsNoBody
      )
    )
  ).foreach {
    case (input, expectedOutput) =>
      test(s"generateIR($input)") {
        val actualOutput = IRBuilder(
          contents = input,
          fileName = "<testfile>",
        )
        assert(actualOutput == expectedOutput)
      }
  }
}
