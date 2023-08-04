package com.github.distcompiler.dcal.parsing

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.given
import cats.*

trait ErrorOps[Elem, Input, Error] {
  def unexpectedEOF(input: Input): Error
  def expectedEOF(input: Input, actualElem: Elem): Error
}
