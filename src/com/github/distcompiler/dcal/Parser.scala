package com.github.distcompiler.dcal

import cats.*
import cats.syntax.all.given
import cats.data.NonEmptyChain

import parsing.{Ps, SourceLocation, InputOps}
import util.EvalList
import AST.*

object Parser {
  private type Elem = Ps[Token]
  private type Error = NonEmptyChain[ParserError]
  private type Input = parsing.TokenInput[Elem, Error]
  private given parsing.ErrorOps[Elem, Error] with {
    override def expectedEOF(sourceLocation: SourceLocation, actualElem: Elem): Error =
      NonEmptyChain.one(ParserError.ExpectedAbstract(category = "EOF", actualTok = actualElem))

    override def unexpectedEOF(sourceLocation: SourceLocation): Error =
      NonEmptyChain.one(ParserError.UnexpectedEOF(sourceLocation))
  }
  private val ops = parsing.Parser.Ops[Elem, Input, Error]
  import ops.*

  private def err(error: ParserError): Error =
    NonEmptyChain.one(error)

  private val anyTok: P[Ps[Token]] = anyElem

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

  private def defParam: P[Ps[AST.DefParam]] =
    ps(name.map(AST.DefParam.Name(_)))

  private def definition: P[Ps[AST.Definition]] =
    capturingPosition {
      kw(Keyword.`def`) ~> ps(name)
      ~ (pn(Punctuation.`(`) ~> repsep(defParam, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`))
      ~ block
    }.mapPositioned {
      case name ~ params ~ body =>
        Ps(AST.Definition(
          name = name,
          params = params.toList,
          body = body,
        ))
    }

  private def intLiteral: P[Ps[AST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.IntLiteral(num)) =>
        Right(Ps(AST.Expression.IntLiteral(num))(using tok.sourceLocation))
      case tok =>
        Left(err(ParserError.ExpectedAbstract(category = "integer literal", actualTok = tok)))
    }

  private def stringLiteral: P[Ps[AST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.StringLiteral(str)) =>
        Right(Ps(AST.Expression.StringLiteral(str))(using tok.sourceLocation))
      case tok =>
        Left(err(ParserError.ExpectedAbstract(category = "string literal", actualTok = tok)))
    }

  private def setConstructorExpr: P[Ps[AST.Expression.SetConstructor]] =
    capturingPosition(pn(Punctuation.`{`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`}`))
      .mapPositioned(members => Ps(AST.Expression.SetConstructor(members.toList)))

  private def opCallExpr: P[Ps[AST.Expression.OpCall]] =
    capturingPosition(path ~ opt(pn(Punctuation.`(`) ~> rep1sep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case name ~ arguments =>
          Ps(AST.Expression.OpCall(Right(name), arguments.map(_.toList).getOrElse(Nil)))
      }

  private def exprBase: P[Ps[AST.Expression]] =
    intLiteral.widenK
    | stringLiteral.widenK
    | opCallExpr.widenK
    | setConstructorExpr.widenK
    | (pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))

  // TODO: precedence
  private def binOpExpr: P[Ps[AST.Expression]] =
    capturingPosition(exprBase ~ opt(ps(BinaryOperator.values.iterator.map(op).reduce(_ | _)) ~ exprBase)).mapPositioned {
      case lhs ~ None => lhs
      case lhs ~ Some(op ~ rhs) =>
        Ps(AST.Expression.OpCall(Left(Ps(op.value.operator)(using op.sourceLocation)), List(lhs, rhs)))
    }
  
  private lazy val expression: P[Ps[AST.Expression]] =
    binOpExpr

  private def await: P[Ps[AST.Statement.Await]] =
    capturingPosition(kw(Keyword.`await`) ~> expression)
      .mapPositioned(expr => Ps(AST.Statement.Await(expr)))

  private def path: P[Ps[AST.Path]] = locally {
    def impl(prefix: Ps[AST.Path]): P[Ps[AST.Path]] = {
      val additions =
        capturingPosition(pn(Punctuation.`[`) ~> expression <~ pn(Punctuation.`]`))
          .mapPositioned(idx => Ps(AST.Path.Index(prefix, idx)))
        | capturingPosition(pn(Punctuation.`.`) ~> name)
          .mapPositioned(proj => Ps(AST.Path.Project(prefix, proj)))

      additions.flatMap(impl) | trivial(prefix)
    }

    capturingPosition(name)
      .mapPositioned(n => Ps(AST.Path.Name(n)))
      .peek
      .flatMap(impl)
  }

  private def assignPair: P[Ps[AST.AssignPair]] =
    capturingPosition(path ~? (pn(Punctuation.`:=`) ~> expression))
      .mapPositioned {
        case lhs ~ rhs => Ps(AST.AssignPair(lhs, rhs))
      }

  private def assignment: P[Ps[AST.Statement.Assignment]] =
    capturingPosition(rep1sep(assignPair, pn(Punctuation.`||`)))
      .mapPositioned(pairs => Ps(AST.Statement.Assignment(pairs.toList)))

  private def call: P[Ps[AST.Binding.Call]] =
    capturingPosition(kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case path ~ arguments => Ps(AST.Binding.Call(path, arguments.toList))
      }

  private lazy val binding: P[Ps[AST.Binding]] =
    ((rep1(capturingPosition(op(BinaryOperator.`\\in`))).left | capturingPosition(op(BinaryOperator.`=`)).right) ~ (expression.left | call.right))
      .map {
        case signifier ~ valuePart =>
          def decorate(inner: Option[SourceLocation] => Ps[AST.Binding]): Ps[AST.Binding] =
            signifier match {
              case Left(selects) =>
                selects.iterator.foldRight(inner(None)) { (select, inner) =>
                  Ps(AST.Binding.Selection(inner))(using select._2)
                }
              case Right(equals) => inner(Some(equals._2))
            }

          decorate { posOpt =>
            valuePart match {
              case Left(expr) =>
                Ps(AST.Binding.Value(expr))(using posOpt.getOrElse(expr.sourceLocation))
              case Right(call) => call.widen
            }
          }
      }

  private def letStmt: P[Ps[AST.Statement.Let]] =
    capturingPosition(kw(Keyword.`let`) ~> ps(name) ~ binding)
      .mapPositioned {
        case name ~ binding => Ps(AST.Statement.Let(name, binding))
      }

  private def varStmt: P[Ps[AST.Statement.Var]] =
    capturingPosition(kw(Keyword.`var`) ~> ps(name) ~ binding)
      .mapPositioned {
        case name ~ binding => Ps(AST.Statement.Var(name, binding))
      }

  private def ifStmt: P[Ps[AST.Statement.If]] =
    capturingPosition {
      (kw(Keyword.`if`) ~> pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))
      ~ block
      ~ opt(kw(Keyword.`else`) ~> block)
    }
      .mapPositioned {
        case condition ~ thenBlock ~ elseBlockOpt =>
          Ps(AST.Statement.If(condition, thenBlock, elseBlockOpt))
      }

  private def callStmt: P[Ps[AST.Statement.Call]] =
    capturingPosition(kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case path ~ arguments =>
          Ps(AST.Statement.Call(Ps(AST.Binding.Call(path, arguments.toList))))
      }

  private lazy val statement: P[Ps[AST.Statement]] =
    await.widenK
    | assignment.widenK
    | letStmt.widenK
    | varStmt.widenK
    | ifStmt.widenK
    | callStmt.widenK
    | block.widenK

  private lazy val block: P[Ps[AST.Statement.Block]] =
    capturingPosition(pn(Punctuation.`{`) ~> rep(statement) <~ pn(Punctuation.`}`))
      .mapPositioned(stmts => Ps(AST.Statement.Block(stmts.toList)))

  private def `import`: P[Ps[AST.Import]] =
    ps(name.map(AST.Import.Name(_)))

  private def imports: P[List[Ps[AST.Import]]] =
    kw(Keyword.`import`) ~> rep1sep(`import`, pn(Punctuation.`,`)).map(_.toList)

  private def module: P[Ps[AST.Module]] =
    capturingPosition(kw(Keyword.`module`) ~> ps(name) ~ opt(imports) ~ rep(definition)).mapPositioned {
      case name ~ importsOpt ~ definitions =>
        Ps(AST.Module(
          name = name,
          imports = importsOpt.getOrElse(Nil),
          definitions = definitions.toList,
        ))
    }

  def apply(tokens: EvalList[Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]], path: String, offsetStart: Int = 0): Either[NonEmptyChain[ParserError], Ps[AST.Module]] = {
    val result = phrase(module).parse(parsing.TokenInput(
      tokens = tokens.map {
        case Left(tokErrs) => Left(NonEmptyChain.one(ParserError.FromTokenizer(tokErrs)))
        case Right(tok) => Right(tok)
      },
      initialSourceLocation = SourceLocation.fileStart(path, offsetStart = offsetStart),
      elemLocation = _.sourceLocation,
    ))
    result match {
      case Left(errors) => Left(errors)
      case Right((module, _)) => Right(module)
    }
  }
}