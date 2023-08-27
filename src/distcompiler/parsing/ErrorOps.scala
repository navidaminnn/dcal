package distcompiler.parsing

import cats.data.{Chain, NonEmptyChain}
import cats.syntax.all.given
import cats.*

trait ErrorOps[Elem, Error] {
  def unexpectedEOF(sourceLocation: SourceLocation): Error
  def expectedEOF(sourceLocation: SourceLocation, actualElem: Elem): Error
}
