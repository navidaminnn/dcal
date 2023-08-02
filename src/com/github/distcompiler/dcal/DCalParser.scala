package com.github.distcompiler.dcal

import parsing.{Ps, SourceLocation}
import cats.data.NonEmptyChain

import com.github.distcompiler.dcal.DCalAST.*
import com.github.distcompiler.dcal.DCalTokenizer.*
import com.github.distcompiler.dcal.parsing.InputOps
import InputOps.LazyListInput
import cats.data.NonEmptyChainImpl.Type

object DCalParser {
  enum ParserError {
    case FromTokenizer(errors: NonEmptyChain[Ps[TokenizerError]])
    case UnexpectedEOF(sourceLocation: SourceLocation)
    case ExpectedAbstract(category: String, actualTok: Ps[Token])
    case ExpectedKeyword(expectedKeyword: Keyword, actualTok: Ps[Token])
    case ExpectedPunctuation(expectedPunctuation: Punctuation, actualTok: Ps[Token])
    case ExpectedBinaryOperator(expectedOperator: BinaryOperator, actualTok: Ps[Token])
  }

  private type Elem = Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]
  private type Error = NonEmptyChain[ParserError]
  private type Input = parsing.InputOps.LazyListInput[Elem]
  private given parsing.ErrorOps.SingleErrorOps[Elem, Input, ParserError] with {
    private def tokErrOr(elem: Elem)(fn: Ps[Token] => ParserError): ParserError =
      elem match {
        case Left(errors) => ParserError.FromTokenizer(errors)
        case Right(actualTok) => fn(actualTok)
      }

    override def expectedEOF(input: Input, actualElem: Elem): ParserError =
      tokErrOr(actualElem) { actualTok =>
        ParserError.ExpectedAbstract(category = "EOF", actualTok = actualTok)
      }

    override def unexpectedEOF(input: Input): ParserError =
      ParserError.UnexpectedEOF(input.prevSourceLocation)
  }
  given inputOps: parsing.InputOps[Elem, Input] with {
    override def getIndex(input: LazyListInput[Either[Type[Ps[TokenizerError]], Ps[Token]]]): Int =
      input.prevSourceLocation.offsetStart

    override def getPrevSourceLocation(input: LazyListInput[Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]]): SourceLocation =
      input.prevSourceLocation

    override def read(input: LazyListInput[Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]]): Option[(Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]], LazyListInput[Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]])] =
      if(input.list.isEmpty) {
        None
      } else {
        Some((input.list.head, input.advanceWithPrevSourceLocation {
          input.list.head match {
            case Left(_) => input.prevSourceLocation
            case Right(value) => value.sourceLocation
          }
        }))
      }
  }
  given parsing.Parser.CapturePrePosition[Elem, Input] with {
    override def capture(input: LazyListInput[Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]]): SourceLocation =
      if(input.list.isEmpty) {
        input.prevSourceLocation
      } else {
        input.list.head match {
          case Left(_) => input.prevSourceLocation
          case Right(value) => value.sourceLocation
        }
      }
  }
  private val ops = parsing.Parser.Ops[Elem, Input, Error]
  import ops.*

  private def err(error: ParserError): Error =
    NonEmptyChain.one(error)

  private val anyTok: P[Ps[Token]] =
    anyElem.constrain {
      case Left(errors) => Left(err(ParserError.FromTokenizer(errors)))
      case Right(tok) => Right(tok)
    }

  private def kw(keyword: Keyword): P[Token.Keyword] =
    anyTok.constrain {
      case Ps(tok @ Token.Keyword(`keyword`)) => Right(tok)
      case tok => Left(err(ParserError.ExpectedKeyword(expectedKeyword = keyword, actualTok = tok)))
    }

  private def pn(punctuation: Punctuation): P[Token.Punctuation] =
    anyTok.constrain {
      case Ps(tok @ Token.Punctuation(`punctuation`)) => Right(tok)
      case tok => Left(err(ParserError.ExpectedPunctuation(expectedPunctuation = punctuation, actualTok = tok)))
    }

  private def op(operator: BinaryOperator): P[Token.BinaryOperator] =
    anyTok.constrain {
      case Ps(tok @ Token.BinaryOperator(`operator`)) => Right(tok)
      case tok => Left(err(ParserError.ExpectedBinaryOperator(expectedOperator = operator, actualTok = tok)))
    }

  private def ps[T](parser: P[T]): P[Ps[T]] =
    capturingPosition(parser).mapPositioned(Ps(_))

  private def name: P[String] =
    anyTok.constrain {
      case Ps(Token.Name(name)) => Right(name)
      case tok => Left(err(ParserError.ExpectedAbstract(category = "name", actualTok = tok)))
    }

  private def imports: P[List[Ps[String]]] =
    kw(Keyword.`import`) ~> rep1sep(ps(name), pn(Punctuation.`,`)).map(_.toList)

  private def definition: P[Ps[DCalAST.Definition]] =
    capturingPosition {
      kw(Keyword.`def`) ~> ps(name)
      ~ (pn(Punctuation.`(`) ~> repsep(ps(name), pn(Punctuation.`,`)) <~ pn(Punctuation.`)`))
      ~ block
    }.mapPositioned {
      case name ~ params ~ body =>
        Ps(DCalAST.Definition(
          name = name,
          params = params.toList,
          body = body,
        ))
    }

  private def intLiteral: P[Ps[DCalAST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.IntLiteral(num)) =>
        Right(Ps(DCalAST.Expression.IntLiteral(num))(using tok.sourceLocation))
      case tok =>
        Left(err(ParserError.ExpectedAbstract(category = "integer literal", actualTok = tok)))
    }

  private def stringLiteral: P[Ps[DCalAST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.StringLiteral(str)) =>
        Right(Ps(DCalAST.Expression.StringLiteral(str))(using tok.sourceLocation))
      case tok =>
        Left(err(ParserError.ExpectedAbstract(category = "string literal", actualTok = tok)))
    }

  private def setConstructorExpr: P[Ps[DCalAST.Expression.SetConstructor]] =
    capturingPosition(pn(Punctuation.`{`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`}`))
      .mapPositioned(members => Ps(DCalAST.Expression.SetConstructor(members.toList)))

  private def opCallExpr: P[Ps[DCalAST.Expression.OpCall]] =
    capturingPosition(path ~ opt(pn(Punctuation.`(`) ~> rep1sep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case name ~ arguments =>
          Ps(DCalAST.Expression.OpCall(Right(name), arguments.map(_.toList).getOrElse(Nil)))
      }

  private def exprBase: P[Ps[DCalAST.Expression]] =
    (intLiteral
    | stringLiteral
    | opCallExpr
    | setConstructorExpr
    | (pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))).map(_.up)

  // TODO: precedence
  private def binOpExpr: P[Ps[DCalAST.Expression]] =
    capturingPosition(exprBase ~ opt(ps(BinaryOperator.values.iterator.map(op).reduce(_ | _)) ~ exprBase)).mapPositioned {
      case lhs ~ None => lhs
      case lhs ~ Some(op ~ rhs) =>
        Ps(DCalAST.Expression.OpCall(Left(Ps(op.value.operator)(using op.sourceLocation)), List(lhs, rhs)))
    }
  
  private lazy val expression: P[Ps[DCalAST.Expression]] =
    binOpExpr

  private def await: P[Ps[DCalAST.Statement.Await]] =
    capturingPosition(kw(Keyword.`await`) ~> expression)
      .mapPositioned(expr => Ps(DCalAST.Statement.Await(expr)))

  private def path: P[Ps[DCalAST.Path]] = locally {
    def impl(prefix: Ps[DCalAST.Path]): P[Ps[DCalAST.Path]] = {
      val additions =
        capturingPosition(pn(Punctuation.`[`) ~> expression <~ pn(Punctuation.`]`))
          .mapPositioned(idx => Ps(DCalAST.Path.Index(prefix, idx)))
        | capturingPosition(pn(Punctuation.`.`) ~> name)
          .mapPositioned(proj => Ps(DCalAST.Path.Project(prefix, proj)))

      additions.flatMap(impl) | trivial(prefix)
    }

    capturingPosition(name)
      .mapPositioned(n => Ps(DCalAST.Path.Name(n)))
      .peek
      .flatMap(impl)
  }

  private def assignPair: P[Ps[DCalAST.AssignPair]] =
    capturingPosition(path ~? (pn(Punctuation.`:=`) ~> expression))
      .mapPositioned {
        case lhs ~ rhs => Ps(DCalAST.AssignPair(lhs, rhs))
      }

  private def assignment: P[Ps[DCalAST.Statement.Assignment]] =
    capturingPosition(rep1sep(assignPair, pn(Punctuation.`||`)))
      .mapPositioned(pairs => Ps(DCalAST.Statement.Assignment(pairs.toList)))

  private def call: P[Ps[DCalAST.Binding.Call]] =
    capturingPosition(kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case path ~ arguments => Ps(DCalAST.Binding.Call(path, arguments.toList))
      }

  private lazy val binding: P[Ps[DCalAST.Binding]] =
    ((rep1(capturingPosition(op(BinaryOperator.`\\in`))).left | capturingPosition(op(BinaryOperator.`=`)).right) ~ (expression.left | call.right))
      .map {
        case signifier ~ valuePart =>
          def decorate(inner: Option[SourceLocation] => Ps[DCalAST.Binding]): Ps[DCalAST.Binding] =
            signifier match {
              case Left(selects) =>
                selects.iterator.foldRight(inner(None)) { (select, inner) =>
                  Ps(DCalAST.Binding.Selection(inner))(using select._2)
                }
              case Right(equals) => inner(Some(equals._2))
            }

          decorate { posOpt =>
            valuePart match {
              case Left(expr) =>
                Ps(DCalAST.Binding.Value(expr))(using posOpt.getOrElse(expr.sourceLocation))
              case Right(call) => call.up
            }
          }
      }

  private def letStmt: P[Ps[DCalAST.Statement.Let]] =
    capturingPosition(kw(Keyword.`let`) ~> ps(name) ~ binding)
      .mapPositioned {
        case name ~ binding => Ps(DCalAST.Statement.Let(name, binding))
      }

  private def varStmt: P[Ps[DCalAST.Statement.Var]] =
    capturingPosition(kw(Keyword.`var`) ~> ps(name) ~ binding)
      .mapPositioned {
        case name ~ binding => Ps(DCalAST.Statement.Var(name, binding))
      }

  private def ifStmt: P[Ps[DCalAST.Statement.If]] =
    capturingPosition {
      (kw(Keyword.`if`) ~> pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))
      ~ block
      ~ opt(kw(Keyword.`else`) ~> block)
    }
      .mapPositioned {
        case condition ~ thenBlock ~ elseBlockOpt =>
          Ps(DCalAST.Statement.If(condition, thenBlock, elseBlockOpt))
      }

  private def callStmt: P[Ps[DCalAST.Statement.Call]] =
    capturingPosition(kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case path ~ arguments =>
          Ps(DCalAST.Statement.Call(Ps(DCalAST.Binding.Call(path, arguments.toList))))
      }

  private lazy val statement: P[Ps[DCalAST.Statement]] =
    (await
    | assignment
    | letStmt
    | varStmt
    | ifStmt
    | callStmt
    | block).map(_.up)

  private lazy val block: P[Ps[DCalAST.Statement.Block]] =
    capturingPosition(pn(Punctuation.`{`) ~> rep(statement) <~ pn(Punctuation.`}`))
      .mapPositioned(stmts => Ps(DCalAST.Statement.Block(stmts.toList)))

  private def module: P[Ps[DCalAST.Module]] =
    capturingPosition(kw(Keyword.`module`) ~> ps(name) ~ opt(imports) ~ rep(definition)).mapPositioned {
      case name ~ importsOpt ~ definitions =>
        Ps(DCalAST.Module(
          name = name,
          imports = importsOpt.getOrElse(Nil),
          definitions = definitions.toList,
        ))
    }

  def apply(tokens: Iterator[Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]], path: String, offsetStart: Int = 0): Either[NonEmptyChain[ParserError], Ps[DCalAST.Module]] = {
    val result = phrase(module).parse(parsing.InputOps.LazyListInput(
      path = path,
      offsetStart = offsetStart,
      list = LazyList.from(tokens),
    ))
    result match {
      case Left(errors) => Left(errors)
      case Right((module, _)) => Right(module)
    }
  }
}