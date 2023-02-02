package com.github.distcompiler.dcal

import scala.collection.View

object DCalTokenizer {
  final case class Position(fileName: String, line: Int, column: Int)

  def annotateCharactersWithPosition(chars: View[Char], fileName: String, startLine: Int = 1, startColumn: Int = 1): View[(Char, Position)] =
    chars
      .scanLeft((startLine, startColumn - 1, None: Option[Char])) { (acc, char) =>
        val (prevLine, prevColumn, _) = acc
        char match {
          case '\n' => (prevLine + 1, 0, Some('\n'))
          case char => (prevLine, prevColumn + 1, Some(char))
        }
      }
      .collect {
        case (line, column, Some(char)) => (char, Position(fileName = fileName, line = line, column = column))
      }

  final case class Token(position: Position, data: TokenData)

  enum TokenData {
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case Name(name: String)
  }

  def tokenize(annotatedChars: View[(Char, Position)]): List[Token] = ???
}
