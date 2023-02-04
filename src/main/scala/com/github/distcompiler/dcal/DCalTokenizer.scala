package com.github.distcompiler.dcal

import scala.collection.View
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input
import scala.util.parsing.input.{NoPosition, Reader}

object DCalTokenizer {
  final case class Position(fileName: String, line: Int, column: Int) {
    def prev: Position =
      (line, column) match {
        case (line, 0) => ???
        case (line, column) => Position(fileName, line, column - 1)
      }
  }

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

  final case class Token(startPosition: Position, endPosition: Position, data: TokenData)

  enum TokenData {
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case Name(name: String)
  }

  class CharPosReader(elems: LazyList[(Char,Position)]) extends Reader[(Char,Position)] {
    override def first: (Char, Position) = elems.head

    override def rest: Reader[(Char, Position)] = new CharPosReader(elems.tail)

    override def atEnd: Boolean = elems.isEmpty

    override def pos: input.Position = NoPosition
  }

  object tokenize extends Parsers {
    override type Elem = (Char,Position)

    def char(ch: Char): Parser[Char] =
      elem(ch.toString, { case (elemCh, _) => ch == elemCh })
        .map(_._1)

    def charElem(kind: String)(pred: Char => Boolean): Parser[Char] =
      elem(kind, { case (elemCh, _) => pred(elemCh) })
        .map(_._1)

    def withPosition(dataParser: Parser[TokenData]): Parser[Token] =
      for {
        (_, startPosition) <- guard(elem("any pair", _ => true))
        data <- dataParser
        // TODO: this idea is broken. come up with better idea
        (_, endPosition) <- guard(elem("any pair", _ => true))
      } yield Token(startPosition = startPosition, endPosition = endPosition.prev, data = data)

    val intLiteral: Parser[Token] =
      withPosition {
        val digit: Parser[Char] = charElem("digit")(ch => ch <= '9' && ch >= '0')
        rep1(digit).map { digits =>
          TokenData.IntLiteral(BigInt.apply(digits.mkString))
        }
      }

    def apply(annotatedChars: View[(Char, Position)]): List[Token] = {
      val annotatedCharsList = annotatedChars.to(LazyList)
      // TODO: special-cased to the one test case. add whitespace skipping and other stuff
      intLiteral.map(List(_)).apply(new CharPosReader(annotatedCharsList)) match {
        case Success(result, _) => result
        case Failure(msg, _) => ???
        case Error(msg, _) => ???
      }
    }
  }
}
