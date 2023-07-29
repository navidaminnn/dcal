package com.github.distcompiler.dcal.parsing

import cats.data.{Chain,NonEmptyChain}

trait ErrorOps[Elem, Input, Error] {
  def combine(left: Error, right: Error): Error

  def unexpectedEOF(input: Input): Error
  def expectedEOF(input: Input, actualElem: Elem): Error
}

object ErrorOps {
  trait SingleErrorOps[Elem, Input, Error] {
    def unexpectedEOF(input: Input): Error
    def expectedEOF(input: Input, actualElem: Elem): Error
  }

  given chainErrorOps[Elem, Input, Error](using singleErrorOps: SingleErrorOps[Elem, Input, Error]): ErrorOps[Elem, Input, Chain[Error]] with {
    override def unexpectedEOF(input: Input): Chain[Error] = Chain.one(singleErrorOps.unexpectedEOF(input))
    override def expectedEOF(input: Input, actualElem: Elem): Chain[Error] =
      Chain.one(singleErrorOps.expectedEOF(input, actualElem))

    override def combine(left: Chain[Error], right: Chain[Error]): Chain[Error] = left ++ right
  }

  given nonEmptyChainErrorOps[Elem, Input, Error](using singleErrorOps: SingleErrorOps[Elem, Input, Error]): ErrorOps[Elem, Input, NonEmptyChain[Error]] with {
    override def unexpectedEOF(input: Input): NonEmptyChain[Error] = NonEmptyChain.one(singleErrorOps.unexpectedEOF(input))
    override def expectedEOF(input: Input, actualElem: Elem): NonEmptyChain[Error] =
      NonEmptyChain.one(singleErrorOps.expectedEOF(input, actualElem))

    override def combine(left: NonEmptyChain[Error], right: NonEmptyChain[Error]): NonEmptyChain[Error] = left ++ right
  }
}
