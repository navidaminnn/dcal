package com.github.distcompiler.dcal.parsing

import com.github.distcompiler.dcal.util.EvalList

final class CharInput private (val lastSourceLocation: SourceLocation, chars: EvalList[Char]) {
  def read: Option[(Char, CharInput)] =
    chars.uncons.map {
      case (char, restChars) =>
        (char, new CharInput(lastSourceLocation.shiftRight, restChars))
    }
}

object CharInput {
  def apply(chars: IterableOnce[Char], path: String, offsetStart: Int = 0): CharInput =
    new CharInput(
      lastSourceLocation = SourceLocation(path, offsetStart, offsetStart),
      chars = EvalList.fromIterableOnce(chars),
    )

  given inputOps[Error]: InputOps[Char, CharInput, Error] with {
    def lastSourceLocation(input: CharInput): SourceLocation =
      input.lastSourceLocation

    def nextSourceLocation(input: CharInput): SourceLocation =
      input.lastSourceLocation.shiftRight

    def read(input: CharInput): Either[Error, Option[(Char, CharInput)]] =
      Right(input.read)
  }
}
