package com.github.distcompiler.dcal

object TestData {
  val baseModule = DCalAST.Module(
    name = "TestModule1",
    imports = Nil,
    definitions = Nil
  )

  val moduleWithImports = DCalAST.Module(
    name = "TestModule1",
    imports = List("TestModule2"),
    definitions = Nil
  )
}
