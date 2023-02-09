package com.github.distcompiler.dcal

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable
import scala.collection.immutable.List

class TestDCalParser extends AnyFunSuite {
  import DCalParser.*

  def fileContent(lines: String*): String = lines.mkString("\n")

  val aModule = "module TestModule1"
  val anImport = "import TestModule2"
  val argNoBlockDef = "def aFunc (anArg) {}"

  List(
    aModule -> AST.Module(
      name = "TestModule1",
      imports = Nil,
      definitions = Nil
    ),
    fileContent(aModule, anImport) -> AST.Module(
      name = "TestModule1",
      imports = List("TestModule2"),
      definitions = Nil
    ),
    fileContent(aModule, anImport, argNoBlockDef) -> AST.Module(
      name = "TestModule1",
      imports = List("TestModule2"),
      definitions = List(
        AST.Definition(
          name = "aFunc",
          args = List("anArg"),
          block = AST.Block(statements = Nil)
        )
      )
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
