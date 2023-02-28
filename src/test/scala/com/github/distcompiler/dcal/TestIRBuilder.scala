package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

class TestIRGenerator extends AnyFunSuite {
  List(
    "module TestModule1" -> IR.Module(
      name = "TestModule1", definitions = Nil
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
