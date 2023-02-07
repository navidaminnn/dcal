package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.TokenData._

import scala.collection.View
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input
import scala.util.parsing.input.{NoPosition, Reader}

object DCalTokenizer {
  final case class Position(fileName: String, line: Int, column: Int) extends input.Position {
    protected override def lineContents: String = ??? // should be unreachable
  }

  /**
   * A token decorated with position info
   *
   * @param startPosition token starting line and column
   * @param endPosition   token ending line and column (inclusive)
   * @param data          token data, one of TokenData enum
   */
  final case class Token(startPosition: Position, endPosition: Position, data: TokenData)

  class CharsReader(fileName: String, elems: LazyList[Char], line: Int, column: Int, val lastPos: Position)
    extends Reader[Char] {
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

    def str(str: String): Parser[String] =
      str.view
        .map(elem)
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

    private val numeric: Parser[Char] = elem("numeric", ch =>
      ch <= '9' && ch >= '0')

    private val alphabetic: Parser[Char] = elem("alphabetic", ch =>
      (ch <= 'z' && ch >= 'a') || (ch <= 'Z' && ch >= 'A'))

    private val intLiteral: Parser[Token] =
      withPosition {
        rep1(numeric).map { digits =>
          TokenData.IntLiteral(BigInt.apply(digits.mkString))
        }
      }

    private val stringLiteral: Parser[Token] =
    withPosition {
      val character: Parser[Char] = elem("character", ch => ch != '"' && ch != '\\') |
        elem('\\') ~> acceptMatch("escape sequence", {
          case '\\' => '\\'
          case '"' => '\"'
          case 'n' => '\n'
          case 't' => '\t'
        })
      (elem('"') ~> rep(character) <~ elem('"'))
        .map(characters => TokenData.StringLiteral(characters.mkString))
    }

    private val name: Parser[Token] =
    withPosition {
      val character: Parser[Char] = elem("underscore", ch => ch == '_') | alphabetic | numeric
      rep1(character).map(characters => TokenData.Name(characters.mkString))
    }

    private val openCurlyBracket: Parser[Token] =
      withPosition{
        elem('{').map( _ => OpenCurlyBracket )
      }

    private val closedCurlyBracket: Parser[Token] =
      withPosition{
        elem('}').map( _ => CloseCurlyBracket )
      }

    private val let: Parser[Token] =
      withPosition({
        str("let").map(_ => TokenData.Let )
      })

    private val `var`: Parser[Token] =
      withPosition({
        str("var").map(_ => TokenData.Var)
      })

    private val equals: Parser[Token] =
      withPosition{
        elem('=').map( _ => TokenData.Equals)
      }

    private val walrus: Parser[Token] =
      withPosition({
        str(":=").map(_ => TokenData.Walrus)
      })

    private val doublePipe: Parser[Token] =
      withPosition {
        str("||").map(_ => TokenData.DoublePipe)
      }

    private val slashIn: Parser[Token] =
      withPosition {
        str("\\in").map(_ => TokenData.SlashIn)
      }

    private val await: Parser[Token] =
      withPosition {
        str("await").map(_ => TokenData.Await)
      }

    private val `def`: Parser[Token] =
      withPosition {
        str("def").map(_ => TokenData.Def)
      }

    private val `import`: Parser[Token] =
      withPosition {
        str("import").map(_ => TokenData.Import)
      }

    private val module: Parser[Token] =
      withPosition {
        str("module").map(_ => TokenData.Module)
      }

    private val openParenthesis: Parser[Token] =
      withPosition {
        elem('(').map(_ => TokenData.OpenParenthesis)
      }

    private val closeParenthesis: Parser[Token] =
      withPosition {
        elem(')').map(_ => TokenData.ClosParenthesis)
      }

    private val comma: Parser[Token] =
      withPosition {
        elem(',').map(_ => TokenData.Comma)
      }

    private val whitespace: Parser[Unit] =
      (
        elem(' ') | elem('\n') | elem('\t') | elem('\r') | elem('\f')
        )
        .map(_ => ())

    private val singleToken: Parser[Option[Token]] = {
      val ps = Map(
        intLiteral -> 1,
        stringLiteral -> """""""".length,
        name -> 1,
        openCurlyBracket -> 1,
        closedCurlyBracket -> 1,
        let -> "let".length,
        `var` -> "var".length,
        equals -> 1,
        walrus -> 1,
        doublePipe -> "||".length,
        slashIn -> "\\in".length,
        await -> "await".length,
        `def` -> "def".length,
        `import` -> "import".length,
        module -> "module".length,
        openParenthesis -> 1,
        closeParenthesis -> 1,
        comma -> 1
      ).toList.sortWith(_._2 > _._2).map( { case (p, _) => p } )
      ps.fold(intLiteral) { (p, _p) =>
        p | _p
      }.map(Some(_)) |
        whitespace.map(_ => None)
    }

    def apply(chars: IterableOnce[Char],
              startLine: Int = 1, startColumn: Int = 1,
              fileName: String): Iterator[Token] =
      Iterator.unfold(new CharsReader(
        fileName = fileName,
        elems = chars.iterator.to(LazyList),
        line = startLine,
        column = startColumn,
        lastPos = Position(fileName = fileName, line = startLine, column = startColumn),
      )) { reader =>
        if (reader.atEnd) {
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
