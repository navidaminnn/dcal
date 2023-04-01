package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.{DCalErrors, DCalParser, DCalScopeAnalyzer, NameNotFound}
import org.scalatest.funsuite.AnyFunSuite

class DCalScopeAnalyzerTest extends AnyFunSuite {
  List(
    """module MyTest
      |def resetString() { str := "new string"; }
      |""".stripMargin -> None,
    """module MyTest
      |def resetString() { str := "new string"; }
      |def resetString() { str := "another new string"; }
      |""".stripMargin -> Some(DCalErrors(RedefinedName("resetString"))),
    """module MyTest
      |def resetString() { helloworld := "new string"; }
      |""".stripMargin -> Some(DCalErrors(NameNotFound("helloworld"))),
    """module MyTest
      |def resetString(p) { p := "new string"; }
      |""".stripMargin -> Some(DCalErrors(ReassignmentToImmutable("p"))),
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
