package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalTokenizer.Token

import scala.collection.View

object DCalParser {
  def parse(tokens: View[Token]): AST.DCalModule = ???

  def apply(contents: String): AST.DCalModule = ???
}
