package com.github.distcompiler.dcal.parsing

trait InputOps[Elem, Input, Error] {
  def lastSourceLocation(input: Input): SourceLocation
  def nextSourceLocation(input: Input): SourceLocation
  def read(input: Input): Either[Error, Option[(Elem, Input)]]
}
