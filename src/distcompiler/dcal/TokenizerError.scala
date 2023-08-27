package distcompiler.dcal

enum TokenizerError {
  case ExpectedAbstract(category: String, actualChar: Char)
  case ExpectedExact(expectedChar: Char, actualChar: Char)
  case UnexpectedEOF
}
