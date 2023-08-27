package distcompiler.dcal

import cats.data.NonEmptyChain
import distcompiler.parsing.{SourceLocation, Ps}

enum ParserError {
  case FromTokenizer(errors: NonEmptyChain[Ps[TokenizerError]])
  case UnexpectedEOF(sourceLocation: SourceLocation)
  case ExpectedAbstract(category: String, actualTok: Ps[Token])
  case ExpectedKeyword(expectedKeyword: Keyword, actualTok: Ps[Token])
  case ExpectedPunctuation(expectedPunctuation: Punctuation, actualTok: Ps[Token])
  case ExpectedBinaryOperator(expectedOperator: BinaryOperator, actualTok: Ps[Token])
}
