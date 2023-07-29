package com.github.distcompiler.dcal

import parsing.{Ps, SourceLocation}
import cats.data.NonEmptyChain

import com.github.distcompiler.dcal.DCalAST.*
import com.github.distcompiler.dcal.DCalTokenizer.*
import com.github.distcompiler.dcal.parsing.InputOps
import com.github.distcompiler.dcal.parsing.InputOps.LazyListInput
import cats.data.NonEmptyChainImpl.Type

object DCalParser {
  enum ParserError {
    case FromTokenizer(errors: NonEmptyChain[Ps[TokenizerError]])
    case UnexpectedEOF(sourceLocation: SourceLocation)
    case ExpectedAbstract(category: String, actualTok: Ps[Token])
    case ExpectedKeyword(expectedKeyword: Keyword, actualTok: Ps[Token])
    case ExpectedPunctuation(expectedPunctuation: Punctuation, actualTok: Ps[Token])
    case ExpectedOperator(expectedOperator: Operator, actualTok: Ps[Token])
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
    override def getPrevSourceLocation(input: LazyListInput[Either[Type[Ps[TokenizerError]], Ps[Token]]]): SourceLocation =
      input.prevSourceLocation

    override def read(input: LazyListInput[Either[Type[Ps[TokenizerError]], Ps[Token]]]): Option[(Either[Type[Ps[TokenizerError]], Ps[Token]], LazyListInput[Either[Type[Ps[TokenizerError]], Ps[Token]]])] =
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
    override def capture(input: LazyListInput[Either[Type[Ps[TokenizerError]], Ps[Token]]]): SourceLocation =
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

  private def op(operator: Operator): P[Token.Operator] =
    anyTok.constrain {
      case Ps(tok @ Token.Operator(`operator`)) => Right(tok)
      case tok => Left(err(ParserError.ExpectedOperator(expectedOperator = operator, actualTok = tok)))
    }

  private def ps[T](parser: P[T]): P[Ps[T]] =
    capturingPosition(parser).mapPositioned(Ps(_))

  private val name: P[String] =
    anyTok.constrain {
      case Ps(Token.Name(name)) => Right(name)
      case tok => Left(err(ParserError.ExpectedAbstract(category = "name", actualTok = tok)))
    }

  private val imports: P[List[Ps[String]]] =
    kw(Keyword.`import`) ~> rep1sep(ps(name), pn(Punctuation.`,`)).map(_.toList)

  private val definition: P[Ps[DCalAST.Definition]] =
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

  private val intLiteral: P[Ps[DCalAST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.IntLiteral(num)) =>
        Right(Ps(DCalAST.Expression.IntLiteral(num))(using tok.sourceLocation))
      case tok =>
        Left(err(ParserError.ExpectedAbstract(category = "integer literal", actualTok = tok)))
    }

  private val stringLiteral: P[Ps[DCalAST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.StringLiteral(str)) =>
        Right(Ps(DCalAST.Expression.StringLiteral(str))(using tok.sourceLocation))
      case tok =>
        Left(err(ParserError.ExpectedAbstract(category = "string literal", actualTok = tok)))
    }

  private val pathRefExpr: P[Ps[DCalAST.Expression.PathRef]] =
    capturingPosition(path).mapPositioned(p => Ps(DCalAST.Expression.PathRef(p)))

  private val exprBase: P[Ps[DCalAST.Expression]] =
    intLiteral
    | stringLiteral
    | pathRefExpr
    | (pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))

  // TODO: precedence
  private val binOpExpr: P[Ps[DCalAST.Expression]] =
    capturingPosition(exprBase ~ opt(ps(Operator.values.iterator.map(op).reduce(_ | _)) ~ exprBase)).mapPositioned {
      case lhs ~ None => lhs
      case lhs ~ Some(op ~ rhs) =>
        Ps(DCalAST.Expression.OpCall(Ps(DCalAST.Path.Name(op.value.operator.name))(using op.sourceLocation), List(lhs, rhs)))
    }
  
  private lazy val expression: P[Ps[DCalAST.Expression]] =
    binOpExpr

  private val await: P[Ps[DCalAST.Statement.Await]] =
    capturingPosition(kw(Keyword.`await`) ~> expression)
      .mapPositioned(expr => Ps(DCalAST.Statement.Await(expr)))

  private val path: P[Ps[DCalAST.Path]] = locally {
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

  private val assignPair: P[Ps[DCalAST.AssignPair]] =
    capturingPosition(path ~? (pn(Punctuation.`:=`) ~> expression))
      .mapPositioned {
        case lhs ~ rhs => Ps(DCalAST.AssignPair(lhs, rhs))
      }

  private val assignment: P[Ps[DCalAST.Statement.Assignment]] =
    capturingPosition(rep1sep(assignPair, pn(Punctuation.`||`)))
      .mapPositioned(pairs => Ps(DCalAST.Statement.Assignment(pairs.toList)))

  private val call: P[Ps[DCalAST.Binding.Call]] =
    capturingPosition(kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case path ~ arguments => Ps(DCalAST.Binding.Call(path, arguments.toList))
      }

  private val binding: P[Ps[DCalAST.Binding]] =
    ((rep1(capturingPosition(op(Operator.`\\in`))).left | capturingPosition(op(Operator.`=`)).right) ~ (expression.left | call.right))
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
              case Right(call) => call
            }
          }
      }

  private val letStmt: P[Ps[DCalAST.Statement.Let]] =
    capturingPosition(kw(Keyword.`let`) ~> ps(name) ~ binding)
      .mapPositioned {
        case name ~ binding => Ps(DCalAST.Statement.Let(name, binding))
      }

  private val varStmt: P[Ps[DCalAST.Statement.Var]] =
    capturingPosition(kw(Keyword.`var`) ~> ps(name) ~ binding)
      .mapPositioned {
        case name ~ binding => Ps(DCalAST.Statement.Var(name, binding))
      }

  private val ifStmt: P[Ps[DCalAST.Statement.If]] =
    capturingPosition {
      (kw(Keyword.`if`) ~> pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))
      ~ block
      ~ opt(kw(Keyword.`else`) ~> block)
    }
      .mapPositioned {
        case condition ~ thenBlock ~ elseBlockOpt =>
          Ps(DCalAST.Statement.If(condition, thenBlock, elseBlockOpt))
      }

  private val callStmt: P[Ps[DCalAST.Statement.Call]] =
    capturingPosition(kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case path ~ arguments =>
          Ps(DCalAST.Statement.Call(Ps(DCalAST.Binding.Call(path, arguments.toList))))
      }

  private val statement: P[Ps[DCalAST.Statement]] =
    await
    | assignment
    | letStmt
    | varStmt
    | ifStmt
    | callStmt

  private lazy val block: P[Ps[DCalAST.Statement.Block]] =
    capturingPosition(pn(Punctuation.`{`) ~> rep(statement) <~ pn(Punctuation.`}`))
      .mapPositioned(stmts => Ps(DCalAST.Statement.Block(stmts.toList)))

  private val module: P[Ps[DCalAST.Module]] =
    capturingPosition(kw(Keyword.`module`) ~> ps(name) ~ opt(imports) ~ rep(definition)).mapPositioned {
      case name ~ importsOpt ~ definitions =>
        Ps(DCalAST.Module(
          name = name,
          imports = importsOpt.getOrElse(Nil),
          definitions = definitions.toList,
        ))
    }

  //   // TODO: Operator precedence behaviour needs reworking
  //   lazy val expression: Parser[DCalAST.Expression] =  {
  //     val unOp: Parser[DCalAST.UnOp] =
  //       acceptMatch("unOp", {
  //         case DCalTokenData.Tilda => DCalAST.UnOp.Not
  //       })

  //     val expressionUnOp: Parser[DCalAST.Expression.ExpressionUnOp] =
  //       (unOp ~ expressionBase).map {
  //         case unOp ~ expr => DCalAST.Expression.ExpressionUnOp(unop = unOp, expr = expr)
  //       }

  //     val relOp: Parser[DCalAST.RelOp] =
  //       acceptMatch("relOp", {
  //         case DCalTokenData.EqualTo => DCalAST.RelOp.EqualTo
  //         case DCalTokenData.NotEqualTo => DCalAST.RelOp.NotEqualTo
  //         case DCalTokenData.LesserThan => DCalAST.RelOp.LesserThan
  //         case DCalTokenData.LesserThanOrEqualTo => DCalAST.RelOp.LesserThanOrEqualTo
  //         case DCalTokenData.GreaterThan => DCalAST.RelOp.GreaterThan
  //         case DCalTokenData.GreaterThanOrEqualTo => DCalAST.RelOp.GreaterThanOrEqualTo
  //       })

  //     val expressionRelOp: Parser[DCalAST.Expression.ExpressionRelOp] =
  //       (expressionBase ~ relOp ~ expressionBase).map {
  //         case lhs ~ relOp ~ rhs => DCalAST.Expression.ExpressionRelOp(
  //           lhs = lhs, relOp = relOp, rhs = rhs
  //         )
  //       }

  //     val logicOp: Parser[DCalAST.LogicOp] =
  //       acceptMatch("logicOp", {
  //         case DCalTokenData.Or => DCalAST.LogicOp.Or
  //         case DCalTokenData.And => DCalAST.LogicOp.And
  //       })

  //     val expressionLogicOp: Parser[DCalAST.Expression] =
  //       (expressionBase ~ logicOp ~ expressionBase).map {
  //         case lhs ~ logicOp ~ rhs => DCalAST.Expression.ExpressionLogicOp(
  //           lhs = lhs, logicOp = logicOp, rhs = rhs
  //         )
  //       }

  //     val binOp: Parser[DCalAST.BinOp] =
  //       acceptMatch("binOp", {
  //         case DCalTokenData.Plus => DCalAST.BinOp.Plus
  //         case DCalTokenData.Minus => DCalAST.BinOp.Minus
  //       })

  //     val expressionBinOp: Parser[DCalAST.Expression] =
  //       (expressionBase ~ opt(binOp ~ expressionBase)).map {
  //         case lhs ~ rhsOpt =>
  //           rhsOpt
  //             .map {
  //               case binOp ~ rhs => DCalAST.Expression.ExpressionBinOp(
  //                 lhs = lhs, binOp = binOp, rhs = rhs
  //               )
  //             }
  //             .getOrElse(lhs)
  //       }

  //     expressionUnOp | expressionRelOp | expressionLogicOp | expressionBinOp
  //   }

  //   lazy val aCall: Parser[DCalAST.aCall] = {
  //     val importedName = delimited(name, elem(DCalTokenData.Dot))
  //     val args = elem(DCalTokenData.OpenParenthesis) ~> opt(delimited(expression)) <~ elem(DCalTokenData.CloseParenthesis)

  //     (name ~ args).map {
  //       case defName ~ args => DCalAST.aCall(
  //         moduleName = Nil,
  //         definitionName = defName,
  //         args = args.getOrElse(Nil)
  //       )
  //     } | (importedName ~ args).map {
  //       case (moduleNames :+ defName) ~ args => DCalAST.aCall(
  //         moduleName = moduleNames, definitionName = defName, args = args.getOrElse(Nil)
  //       )
  //     }
  //   }

  //   lazy val expressionBase: Parser[DCalAST.Expression] = {
  //     lazy val boolean = acceptMatch("boolean", {
  //       case DCalTokenData.True => DCalAST.Expression.True
  //       case DCalTokenData.False => DCalAST.Expression.False
  //     })

  //     val literal = acceptMatch("literal", {
  //       case DCalTokenData.IntLiteral(v) => DCalAST.Expression.IntLiteral(v)
  //       case DCalTokenData.StringLiteral(v) => DCalAST.Expression.StringLiteral(v)
  //     })

  //     val nameExpr = name.map(str => DCalAST.Expression.Name(str))

  //     val bracketedExpr =
  //       (elem(DCalTokenData.OpenParenthesis) ~> expression <~ elem(DCalTokenData.CloseParenthesis)).map { expr => expr }

  //     val set =
  //       (elem(DCalTokenData.OpenCurlyBracket) ~> opt(delimited(expression)) <~ elem(DCalTokenData.CloseCurlyBracket)).map { setMembers =>
  //         DCalAST.Expression.Set(members = setMembers.getOrElse(Nil))
  //       }

  //     bracketedExpr | set | boolean | literal | nameExpr
  //   }
  // }

  def apply(contents: String, fileName: String): DCalAST.Module =
    ???
}