package com.github.distcompiler.dcal

import parsing.{Ps, SourceLocation}

import cats.data.NonEmptyChain

object Tokenizer {
  private type Elem = Char
  private type Error = NonEmptyChain[Ps[TokenizerError]]
  private type Input = parsing.InputOps.LazyListInput[Char]
  private given parsing.ErrorOps[Elem, Input, Error] with {
    override def expectedEOF(input: Input, actualElem: Elem): Error =
      NonEmptyChain.one {
        Ps(TokenizerError.ExpectedAbstract(category = "EOF", actualChar = actualElem))(using input.prevSourceLocation.shiftRight)
      }

    override def unexpectedEOF(input: Input): Error =
      NonEmptyChain.one {
        Ps(TokenizerError.UnexpectedEOF)(using input.prevSourceLocation.shiftRight)
      }
  }
  private val ops = parsing.Parser.Ops[Elem, Input, Error]
  import ops.*

  private def err(error: TokenizerError)(using SourceLocation): Error =
    NonEmptyChain.one(Ps(error))

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

  private val intLiteral: P[Ps[Token]] =
    capturingPosition(rep1(numeric))
      .mapPositioned { digits =>
        Ps(Token.IntLiteral(BigInt(digits.iterator.mkString)))
      }

  private val stringLiteral: P[Ps[Token]] =
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

  private val name: P[Ps[Token]] =
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
      .sortWith(_._1 > _._1)
      .map {
        case (name, fn) =>
          capturingPosition(str(name))
            .mapPositioned(_ => fn)
      }
      .reduce(_ | _)

  private val fixedTokens: P[Ps[Token]] =
    mkLiteralSet {
      Keyword.values.iterator.map(kw => (kw.productPrefix, (loc: SourceLocation) ?=> Ps(Token.Keyword(kw))))
      ++ Punctuation.values.iterator.map(p => (p.productPrefix, (loc: SourceLocation) ?=> Ps(Token.Punctuation(p))))
      ++ BinaryOperator.values.iterator.map(op => (op.productPrefix, (loc: SourceLocation) ?=> Ps(Token.BinaryOperator(op))))
    }

  private val whitespace: P[Unit] =
    capturingPosition(anyElem).constrain {
      case (ch, _) if Character.isWhitespace(ch) => Right(())
      case (ch, given SourceLocation) =>
        Left(err(TokenizerError.ExpectedAbstract(category = "whitespace", actualChar = ch)))
    }

  private val singleTokenOpt: P[Option[Ps[Token]]] =
    ( stringLiteral
    | fixedTokens
    | name
    | intLiteral
    ).map(Some(_))
    | whitespace.map(_ => None)

  def apply(contents: Iterable[Char], path: String, offsetStart: Int = 0): Iterator[Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]] =
    singleTokenOpt
      .parseAll(parsing.InputOps.LazyListInput(list = contents.to(LazyList), path = path, offsetStart = offsetStart))
      .flatMap {
        case Left(errs) => Some(Left(errs))
        case Right(tokenOpt) => tokenOpt.map(Right(_))
      }
}
