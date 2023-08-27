package distcompiler.parsing

import distcompiler.util.EvalList

final class TokenInput[Elem, Error] private (val lastSourceLocation: SourceLocation,
                                             val idx: Int,
                                             val tokens: EvalList[Either[Error, Elem]],
                                             val elemLocation: Elem => SourceLocation) {

  def read: Either[Error, Option[(Elem, TokenInput[Elem, Error])]] =
    tokens.uncons match {
      case None => Right(None)
      case Some((Left(err), restTokens)) =>
        assert(restTokens.isEmpty)
        Left(err)
      case Some((Right(elem), restTokens)) =>
        Right(Some(elem, new TokenInput(
          lastSourceLocation = elemLocation(elem),
          idx = idx + 1,
          tokens = restTokens,
          elemLocation = elemLocation,
        )))
    }
}

object TokenInput {
  private case class Fns[Elem, Error](elemLocation: Elem => SourceLocation, errLocation: Error => SourceLocation)

  def apply[Elem, Error](tokens: EvalList[Either[Error, Elem]], initialSourceLocation: SourceLocation, elemLocation: Elem => SourceLocation): TokenInput[Elem, Error] =
    new TokenInput[Elem, Error](
      lastSourceLocation = initialSourceLocation,
      idx = 0,
      tokens = tokens,
      elemLocation = elemLocation,
    )

  given inputOps[Elem, Error]: InputOps[Elem, TokenInput[Elem, Error], Error] with {
    def lastSourceLocation(input: TokenInput[Elem, Error]): SourceLocation =
      input.lastSourceLocation

    def nextSourceLocation(input: TokenInput[Elem, Error]): SourceLocation =
      input.tokens.uncons match {
        case None | Some((Left(_), _)) => input.lastSourceLocation
        case Some((Right(elem), _)) => input.elemLocation(elem)
      }

    def tellIdx(input: TokenInput[Elem, Error]): Int = input.idx

    def read(input: TokenInput[Elem, Error]): Either[Error, Option[(Elem, TokenInput[Elem, Error])]] = input.read
  }
}
