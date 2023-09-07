package distcompiler.dcal

import distcompiler.util.EvalList
import distcompiler.parsing.{Ps, SourceLocation}

import cats.*
//import cats.syntax.all.given
import cats.data.*

object Tokenizer {
  private type Elem = Char
  private type Error = Ps[TokenizerError]
  private type Input = distcompiler.parsing.CharInput
  private given distcompiler.parsing.ErrorOps[Elem, Error] with {
    override def expectedEOF(sourceLocation: SourceLocation, actualElem: Elem): Error =
      Ps(TokenizerError.ExpectedAbstract(category = "EOF", actualChar = actualElem))(using sourceLocation)

    override def unexpectedEOF(sourceLocation: SourceLocation): Error =
      Ps(TokenizerError.UnexpectedEOF)(using sourceLocation)
  }
  private val ops = distcompiler.parsing.Parser.Ops[Elem, Input, Error]
  import ops.{*, given}

  private def err(error: TokenizerError)(using SourceLocation): Error = Ps(error)

  private def elem(elem: Elem): P[Elem] =
    capturingPosition(anyElem).constrain {
      case (`elem`, _) => Right(elem)
      case (actualElem, given SourceLocation) =>
        Left(err(TokenizerError.ExpectedExact(expectedChar = elem, actualChar = actualElem)))
    }

  private def str(str: String): P[String] =
    str.view
      .map(elem)
      .reduceOption(_ ~>? _)
      .map(_.map(_ => str))
      .getOrElse(trivial(str))

  private val numeric: P[Char] =
    capturingPosition(anyElem).constrain {
      case (ch, _) if Character.isDigit(ch) => Right(ch)
      case (ch, given SourceLocation) =>
        Left(err(TokenizerError.ExpectedAbstract(category = "digit", actualChar = ch)))
    }

  private val alphabetic: P[Char] =
    capturingPosition(anyElem).constrain {
      case (ch, _) if Character.isAlphabetic(ch) => Right(ch)
      case (ch, given SourceLocation) =>
        Left(err(TokenizerError.ExpectedAbstract(category = "alphabetic", actualChar = ch)))
    }

  private val intLiteral: P[Ps[Token.IntLiteral]] =
    capturingPosition(rep1(numeric))
      .mapPositioned { digits =>
        Ps(Token.IntLiteral(BigInt(digits.iterator.mkString)))
      }

  private val stringLiteral: P[Ps[Token.StringLiteral]] =
    capturingPosition {
      val strCharacter: P[Char] = capturingPosition(anyElem).constrain {
        case (ch, _) if ch != '"' && ch != '\\' => Right(ch)
        case (ch, given SourceLocation) => Left(err(TokenizerError.ExpectedAbstract(category = "non-escape string contents", actualChar = ch)))
      }
      val strEscape: P[Char] = capturingPosition(elem('\\') ~> anyElem).constrain {
        case ('\\', _) => Right('\\')
        case ('"', _) => Right('"')
        case ('n', _) => Right('\n')
        case ('t', _) => Right('\t')
        case (ch, given SourceLocation) =>
          Left(err(TokenizerError.ExpectedAbstract(category = "string escape", actualChar = ch)))
      }

      (elem('"') ~> rep(strCharacter | strEscape) <~ elem('"'))
    }.mapPositioned { chars =>
      Ps(Token.StringLiteral(chars.iterator.mkString))
    }

  private val name: P[Ps[Token.Name]] =
    capturingPosition {
      val underscore: P[Char] = elem('_')
      val character: P[Char] = underscore | alphabetic | numeric

      (rep(underscore | numeric) ~? alphabetic ~ rep(character))
    }.mapPositioned {
      case c1 ~ c2 ~ c3 =>
        Ps(Token.Name(s"${c1.iterator.mkString}$c2${c3.iterator.mkString}"))
    }

  private def mkLiteralSet[T](options: Iterator[(String, (SourceLocation ?=> T))]): P[T] =
    options.toArray
      .sortWith(_._1.length() > _._1.length)
      .iterator
      .map {
        case (name, fn) =>
          capturingPosition(str(name))
            .mapPositioned(_ => fn)
      }
      .reduce(_ | _)

  private val fixedTokens: P[Ps[Token]] =
    mkLiteralSet {
      Keyword.values.iterator.map(kw => (kw.name, (loc: SourceLocation) ?=> Ps(Token.Keyword(kw))))
      ++ Punctuation.values.iterator.map(p => (p.name, (loc: SourceLocation) ?=> Ps(Token.Punctuation(p))))
    }

  private val whitespace: P[Unit] =
    capturingPosition(anyElem).constrain {
      case (ch, _) if Character.isWhitespace(ch) => Right(())
      case (ch, given SourceLocation) =>
        Left(err(TokenizerError.ExpectedAbstract(category = "whitespace", actualChar = ch)))
    }

  private val singleTokenOpt: P[Option[Ps[Token]]] =
    ( stringLiteral
    | (fixedTokens ||| name) // if a name is longer, take that even if it contains a token e.g. "ifgnobitz" is a name not the keyword "if" and a name
    | intLiteral
    ).map(Some(_))
    | whitespace.map(_ => None)

  def apply(contents: Iterable[Char], path: String, offsetStart: Int = 0): EvalList[Either[NonEmptyList[Ps[TokenizerError]], Ps[Token]]] =
    singleTokenOpt
      .parseAll(distcompiler.parsing.CharInput(contents, path, offsetStart = offsetStart))
      .collect {
        case Left(errors) => Left(errors)
        case Right(Some(token)) => Right(token)
      }
}
