package com.github.distcompiler.dcal

import parsing.SourceLocation
import cats.data.NonEmptyChain

import com.github.distcompiler.dcal.DCalAST.*
import com.github.distcompiler.dcal.DCalTokenizer.*
import scala.util.CommandLineParser.ParseError

object DCalParser {
  enum ParserError {
    case FromTokenizer(errors: NonEmptyChain[TokenizerError])
    case UnexpectedEOF(sourceLocation: SourceLocation)
    case ExpectedAbstract(category: String, actualTok: Token)
    case ExpectedKeyword(expectedKeyword: Keyword, actualTok: Token)
    case ExpectedPunctuation(expectedPunctuation: Punctuation, actualTok: Token)
    case ExpectedOperator(expectedOperator: Operator, actualTok: Token)
  }

  private type Elem = Either[NonEmptyChain[DCalTokenizer.TokenizerError], Token]
  private type Error = NonEmptyChain[ParserError]
  private type Input = parsing.InputOps.LazyListInput[Elem]
  private given parsing.ErrorOps.SingleErrorOps[Elem, Input, ParserError] with {
    private def tokErrOr(elem: Elem)(fn: Token => ParserError): ParserError =
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
  private val ops = parsing.Parser.Ops[Elem, Input, Error]
  import ops.*

  private def err(error: ParserError): Error =
    NonEmptyChain.one(error)

  private val anyTok: P[Token] =
    anyElem.constrain {
      case Left(errors) => Left(err(ParserError.FromTokenizer(errors)))
      case Right(tok) => Right(tok)
    }

  private def kw(keyword: Keyword): P[Token.Keyword] =
    anyTok.constrain {
      case tok @ Token.Keyword(`keyword`) => Right(tok)
      case tok => Left(err(ParserError.ExpectedKeyword(expectedKeyword = keyword, actualTok = tok)))
    }

  private def pn(punctuation: Punctuation): P[Token.Punctuation] =
    anyTok.constrain {
      case tok @ Token.Punctuation(`punctuation`) => Right(tok)
      case tok => Left(err(ParserError.ExpectedPunctuation(expectedPunctuation = punctuation, actualTok = tok)))
    }

  private def op(operator: Operator): P[Token.Operator] =
    anyTok.constrain {
      case tok @ Token.Operator(`operator`) => Right(tok)
      case tok => Left(err(ParserError.ExpectedOperator(expectedOperator = operator, actualTok = tok)))
    }

  private val name: P[String] =
    anyTok.constrain {
      case Token.Name(name) => Right(name)
      case tok => Left(err(ParserError.ExpectedAbstract(category = "name", actualTok = tok)))
    }

  private val imports: P[List[String]] =
    kw(Keyword.`import`) ~> rep1sep(name, pn(Punctuation.`,`)).map(_.toList)

  private val definition: P[DCalAST.Definition] =
    capturingPosition {
      kw(Keyword.`def`) ~> name
      ~ (pn(Punctuation.`(`) ~> repsep(name, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`))
      ~ block
    }.mapPositioned {
      case name ~ params ~ body =>
        DCalAST.Definition(
          name = name,
          params = params.toList,
          body = body,
        )
    }

  private val intLiteral: P[DCalAST.Expression.IntLiteral] =
    anyTok.constrain {
      case tok @ Token.IntLiteral(num) =>
        Right(DCalAST.Expression.IntLiteral(num)(using tok.sourceLocation))
      case tok =>
        Left(err(ParserError.ExpectedAbstract(category = "integer literal", actualTok = tok)))
    }

  private val stringLiteral: P[DCalAST.Expression.StringLiteral] =
    anyTok.constrain {
      case tok @ Token.StringLiteral(str) =>
        Right(DCalAST.Expression.StringLiteral(str)(using tok.sourceLocation))
      case tok =>
        Left(err(ParserError.ExpectedAbstract(category = "string literal", actualTok = tok)))
    }

  private val pathRefExpr: P[DCalAST.Expression.PathRef] =
    capturingPosition(path).mapPositioned(DCalAST.Expression.PathRef(_))

  private val exprBase: P[DCalAST.Expression] =
    intLiteral
    | stringLiteral
    | pathRefExpr
    | (pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))

  // TODO: precedence
  private val binOpExpr: P[DCalAST.Expression] =
    capturingPosition(exprBase ~ opt(Operator.values.iterator.map(op).reduce(_ | _) ~ exprBase)).mapPositioned {
      case lhs ~ None => lhs
      case lhs ~ Some(op ~ rhs) =>
        DCalAST.Expression.OpCall(DCalAST.Path.Name(op.operator.name)(using op.sourceLocation), List(lhs, rhs))
    }
  
  private lazy val expression: P[DCalAST.Expression] =
    binOpExpr

  private val await: P[DCalAST.Statement.Await] =
    capturingPosition(kw(Keyword.`await`) ~> expression)
      .mapPositioned(DCalAST.Statement.Await(_))

  private val path: P[DCalAST.Path] = locally {
    def impl(prefix: DCalAST.Path): P[DCalAST.Path] = {
      val additions =
        capturingPosition(pn(Punctuation.`[`) ~> expression <~ pn(Punctuation.`]`))
          .mapPositioned(DCalAST.Path.Index(prefix, _))
        | capturingPosition(pn(Punctuation.`.`) ~> name)
          .mapPositioned(DCalAST.Path.Project(prefix, _))

      additions.flatMap(impl) | trivial(prefix)
    }

    capturingPosition(name)
      .mapPositioned(DCalAST.Path.Name(_))
      .peek
      .flatMap(impl)
  }

  private val assignPair: P[DCalAST.AssignPair] =
    capturingPosition(path ~? (pn(Punctuation.`:=`) ~> expression))
      .mapPositioned {
        case lhs ~ rhs => DCalAST.AssignPair(lhs, rhs)
      }

  private val assignment: P[DCalAST.Statement.Assignment] =
    capturingPosition(rep1sep(assignPair, pn(Punctuation.`||`)))
      .mapPositioned(pairs => DCalAST.Statement.Assignment(pairs.toList))

  private val call: P[DCalAST.Binding.Call] =
    capturingPosition(kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case path ~ arguments => DCalAST.Binding.Call(path, arguments.toList)
      }

  private val binding: P[DCalAST.Binding] =
    ((rep1(capturingPosition(op(Operator.`\\in`))).left | capturingPosition(op(Operator.`=`)).right) ~ (expression.left | call.right))
      .map {
        case signifier ~ valuePart =>
          def decorate(inner: Option[SourceLocation] => DCalAST.Binding): DCalAST.Binding =
            signifier match {
              case Left(selects) =>
                selects.iterator.foldRight(inner(None)) { (select, inner) =>
                  DCalAST.Binding.Selection(inner)(using select._2)
                }
              case Right(equals) => inner(Some(equals._2))
            }

          decorate { posOpt =>
            valuePart match {
              case Left(expr) =>
                DCalAST.Binding.Value(expr)(using posOpt.getOrElse(expr.sourceLocation))
              case Right(call) => call
            }
          }
      }

  private val letStmt: P[DCalAST.Statement.Let] =
    capturingPosition(kw(Keyword.`let`) ~> name ~ binding)
      .mapPositioned {
        case name ~ binding => DCalAST.Statement.Let(name, binding)
      }

  private val varStmt: P[DCalAST.Statement.Var] =
    capturingPosition(kw(Keyword.`var`) ~> name ~ binding)
      .mapPositioned {
        case name ~ binding => DCalAST.Statement.Var(name, binding)
      }

  private val ifStmt: P[DCalAST.Statement.If] =
    capturingPosition {
      (kw(Keyword.`if`) ~> pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))
      ~ block
      ~ opt(kw(Keyword.`else`) ~> block)
    }
      .mapPositioned {
        case condition ~ thenBlock ~ elseBlockOpt =>
          DCalAST.Statement.If(condition, thenBlock, elseBlockOpt)
      }

  private val callStmt: P[DCalAST.Statement.Call] =
    capturingPosition(kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case path ~ arguments =>
          DCalAST.Statement.Call(DCalAST.Binding.Call(path, arguments.toList))
      }

  private val statement: P[DCalAST.Statement] =
    await
    | assignment
    | letStmt
    | varStmt
    | ifStmt
    | callStmt

  private lazy val block: P[DCalAST.Statement.Block] =
    capturingPosition(pn(Punctuation.`{`) ~> rep(statement) <~ pn(Punctuation.`}`))
      .mapPositioned(stmts => DCalAST.Statement.Block(stmts.toList))

  private val module: P[DCalAST.Module] =
    capturingPosition(kw(Keyword.`module`) ~> name ~ opt(imports) ~ rep(definition)).mapPositioned {
      case name ~ importsOpt ~ definitions =>
        DCalAST.Module(
          name = name,
          imports = importsOpt.getOrElse(Nil),
          definitions = definitions.toList,
        )
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