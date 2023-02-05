package com.github.distcompiler.dcal

import scala.collection.View
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input
import scala.util.parsing.input.{NoPosition, Reader}

object DCalTokenizer {
  final case class Position(fileName: String, line: Int, column: Int) extends input.Position {
    protected override def lineContents: String = ??? // should be unreachable
  }

  final case class Token(startPosition: Position, endPosition: Position, data: TokenData)

  enum TokenData {
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case Name(name: String)
  }

  class CharsReader(fileName: String, elems: LazyList[Char], line: Int, column: Int, val lastPos: Position) extends Reader[Char] {
    override def first: Char = elems.head

    override def rest: Reader[Char] =
      new CharsReader(
        fileName = fileName,
        elems = elems.tail,
        line = first match {
          case '\n' => line + 1
          case _ => line
        },
        column = first match {
          case '\n' => 1
          case _ => column + 1
        },
        lastPos = pos,
      )

    override def atEnd: Boolean = elems.isEmpty

    override def pos: Position =
      Position(fileName = fileName, line = line, column = column)
  }

  object tokenize extends Parsers {
    override type Elem = Char

    def char(ch: Char): Parser[Char] =
      elem(ch.toString, _ == ch)

    def str(str: String): Parser[String] =
      str.view
        .map(char)
        .reduceOption(_ ~> _)
        .map(_.map(_ => str))
        .getOrElse(success(str))

    def withPosition(dataParser: Parser[TokenData]): Parser[Token] =
      Parser { input =>
        // Our special reader implementation has all the info needed to calculate a position in any situation
        // (even an empty file)
        val startPosition = input.pos.asInstanceOf[Position]
        dataParser(input) match {
          case Success(result, next) =>
            val endPosition = next.asInstanceOf[CharsReader].lastPos
            Success(
              result = Token(startPosition = startPosition, endPosition = endPosition, data = result),
              next = next,
            )
          case noSuccess: NoSuccess => noSuccess
        }
      }

    val intLiteral: Parser[Token] =
      withPosition {
        val digit: Parser[Char] = elem("digit", ch => ch <= '9' && ch >= '0')
        rep1(digit).map { digits =>
          TokenData.IntLiteral(BigInt.apply(digits.mkString))
        }
      }

    val whitespace: Parser[Unit] =
      (
        char(' ') | // TODO: support more whitespace than I need for my tests... comments too?
          char('\n')
      )
        .map(_ => ())

    val singleToken: Parser[Option[Token]] =
      (
        intLiteral // TODO: rest of the tokens go here with |
        ).map(Some(_)) |
        whitespace.map(_ => None)

    def apply(chars: IterableOnce[Char], startLine: Int = 1, startColumn: Int = 1, fileName: String): Iterator[Token] =
      Iterator.unfold(new CharsReader(
        fileName = fileName,
        elems = chars.iterator.to(LazyList),
        line = startLine,
        column = startColumn,
        lastPos = Position(fileName = fileName, line = startLine, column = startColumn),
      )) { reader =>
        if(reader.atEnd) {
          None
        } else {
          singleToken.apply(reader) match {
            case Success(tokenOpt, next) =>
              Some((tokenOpt, next.asInstanceOf[CharsReader]))
            case Failure(msg, _) => ??? // throw exception probably
            case Error(msg, _) => ??? // same
          }
        }
      }
        .flatten
  }
}
