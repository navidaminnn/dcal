package com.github.distcompiler.dcal

import parsing.{SourceLocated, SourceLocation}

import cats.data.NonEmptyChain

object DCalTokenizer {
  enum Token(using SourceLocation) extends SourceLocated {
    case IntLiteral(value: BigInt)(using SourceLocation)
    case StringLiteral(value: String)(using SourceLocation)
    case Name(name: String)(using SourceLocation)
    case Keyword(keyword: DCalTokenizer.Keyword)(using SourceLocation)
    case Punctuation(punctuation: DCalTokenizer.Punctuation)(using SourceLocation)
    case Operator(operator: DCalTokenizer.Operator)(using SourceLocation)
  }

  trait Meta { self: Product =>
    def name: String = productPrefix
  }

  enum Keyword extends Meta {
    case call
    case await
    case `if`
    case `else`
    case `var`
    case `let`
    case `def`
    case `import`
    case `module`
  }

  enum Punctuation extends Meta {
    case `,`
    case `(`
    case `)`
    case `[`
    case `]`
    case `{`
    case `}`
    case `:=`
    case `||`
    case `.`
  }

  enum Operator extends Meta {
    case `\\in`
    case `\\notin`
    case `=`
  }

  enum TokenizerError(using SourceLocation) extends SourceLocated {
    case ExpectedAbstract(category: String, actualChar: Char)(using SourceLocation)
    case ExpectedExact(expectedChar: Char, actualChar: Char)(using SourceLocation)
    case UnexpectedEOF()(using SourceLocation)
  }

  private type Elem = Char
  private type Error = NonEmptyChain[TokenizerError]
  private type Input = parsing.InputOps.LazyListInput[Char]
  private given parsing.ErrorOps.SingleErrorOps[Elem, Input, TokenizerError] with {
    override def expectedEOF(input: Input, actualElem: Elem): TokenizerError =
      TokenizerError.ExpectedAbstract(category = "EOF", actualChar = actualElem)(using input.prevSourceLocation.shiftRight)

    override def unexpectedEOF(input: Input): TokenizerError =
      TokenizerError.UnexpectedEOF()(using input.prevSourceLocation.shiftRight)
  }
  private val ops = parsing.Parser.Ops[Elem, Input, Error]
  import ops.*

  private def err(error: TokenizerError): Error =
    NonEmptyChain.one(error)

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

  private val intLiteral: P[Token] =
    capturingPosition(rep1(numeric))
      .mapPositioned { digits =>
        Token.IntLiteral(BigInt(digits.iterator.mkString))
      }

  private val stringLiteral: P[Token] =
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
      Token.StringLiteral(chars.iterator.mkString)
    }

  private val name: P[Token] =
    capturingPosition {
      val underscore: P[Char] = elem('_')
      val character: P[Char] = underscore | alphabetic | numeric

      (rep(underscore | numeric) ~? alphabetic ~ rep(character))
    }.mapPositioned {
      case c1 ~ c2 ~ c3 =>
        Token.Name(s"${c1.iterator.mkString}$c2${c3.iterator.mkString}")
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

  private val fixedTokens: P[Token] =
    mkLiteralSet {
      Keyword.values.iterator.map(kw => (kw.productPrefix, (loc: SourceLocation) ?=> Token.Keyword(kw)))
      ++ Punctuation.values.iterator.map(p => (p.productPrefix, (loc: SourceLocation) ?=> Token.Punctuation(p)))
      ++ Operator.values.iterator.map(op => (op.productPrefix, (loc: SourceLocation) ?=> Token.Operator(op)))
    }

  private val whitespace: P[Unit] =
    capturingPosition(anyElem).constrain {
      case (ch, _) if Character.isWhitespace(ch) => Right(())
      case (ch, given SourceLocation) =>
        Left(err(TokenizerError.ExpectedAbstract(category = "whitespace", actualChar = ch)))
    }

  private val singleTokenOpt: P[Option[Token]] =
    ( stringLiteral
    | fixedTokens
    | name
    | intLiteral
    ).map(Some(_))
    | whitespace.map(_ => None)

  def apply(contents: Iterable[Char], path: String, offsetStart: Int = 0): Iterator[Either[NonEmptyChain[TokenizerError], Token]] =
    singleTokenOpt
      .parseAll(parsing.InputOps.LazyListInput(list = contents.to(LazyList), path = path, offsetStart = offsetStart))
      .flatMap {
        case Left(errs) => Some(Left(errs))
        case Right(tokenOpt) => tokenOpt.map(Right(_))
      }
}
