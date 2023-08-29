package distcompiler.dcal

import cats.*
import cats.syntax.all.given
import cats.data.*

import distcompiler.parsing.{Ps, SourceLocation, InputOps}
import distcompiler.util.EvalList
import AST.*

object Parser {
  type Elem = Ps[Token]
  type Error = ParserError
  type Input = distcompiler.parsing.TokenInput[Elem, Error]
  given distcompiler.parsing.ErrorOps[Elem, Error] with {
    override def expectedEOF(sourceLocation: SourceLocation, actualElem: Elem): Error =
      ParserError.ExpectedAbstract(category = "EOF", actualTok = actualElem)

    override def unexpectedEOF(sourceLocation: SourceLocation): Error =
      ParserError.UnexpectedEOF(sourceLocation)
  }
  private val ops = distcompiler.parsing.Parser.Ops[Elem, Input, Error]
  import ops.{*, given}

  private val anyTok: P[Ps[Token]] = anyElem

  def kw(keyword: Keyword): P[Token.Keyword] =
    anyTok.constrain {
      case Ps(tok @ Token.Keyword(`keyword`)) => Right(tok)
      case tok => Left(ParserError.ExpectedKeyword(expectedKeyword = keyword, actualTok = tok))
    }

  def pn(punctuation: Punctuation): P[Token.Punctuation] =
    anyTok.constrain {
      case Ps(tok @ Token.Punctuation(`punctuation`)) => Right(tok)
      case tok => Left(ParserError.ExpectedPunctuation(expectedPunctuation = punctuation, actualTok = tok))
    }

  def op(operator: BinaryOperator): P[Token.BinaryOperator] =
    anyTok.constrain {
      case Ps(tok @ Token.BinaryOperator(`operator`)) => Right(tok)
      case tok => Left(ParserError.ExpectedBinaryOperator(expectedOperator = operator, actualTok = tok))
    }

  def ps[T](parser: P[T]): P[Ps[T]] =
    capturingPosition(parser).map((v, pos) => Ps(v)(using pos))

  def parentheses[T](parser: P[T]): P[T] =
    pn(Punctuation.`(`) ~> parser <~ pn(Punctuation.`)`)

  def brackets[T](parser: P[T]): P[T] =
    pn(Punctuation.`[`) ~> parser <~ pn(Punctuation.`]`)

  def braces[T](parser: P[T]): P[T] =
    pn(Punctuation.`{`) ~> parser <~ pn(Punctuation.`}`)

  def commaSep[T](elem: P[T]): P[List[T]] =
    repsep(elem, pn(Punctuation.`,`)).map(_.toList)

  def comma1Sep[T](elem: P[T]): P[List[T]] =
    rep1sep(elem, pn(Punctuation.`,`)).map(_.toList)

  lazy val name: P[String] =
    anyTok.constrain {
      case Ps(Token.Name(name)) => Right(name)
      case tok => Left(ParserError.ExpectedAbstract(category = "name", actualTok = tok))
    }

  lazy val param: P[Ps[AST.Param]] =
    ps {
      ps(name).map(AST.Param.Name.apply)
      | (kw(Keyword.`impl`) ~> path ~ opt(ps(name))).map {
          case interface ~ aliasOpt =>  AST.Param.Impl(interface, aliasOpt)
        }
    }

  lazy val qualifier: P[Ps[AST.Definition.Qualifier]] =
    ps {
      kw(Keyword.`pure`).as(AST.Definition.Qualifier.Pure)
      | kw(Keyword.`async`).as(AST.Definition.Qualifier.Async)
    }

  lazy val definition: P[Ps[AST.Definition]] =
    ps {
      lzy {
        (opt(qualifier)
        ~ (kw(Keyword.`def`) ~> ps(name))
        ~ parentheses(commaSep(param))
        ~ block
        ).map {
          case qualifierOpt ~ name ~ params ~ body =>
            AST.Definition.Def(qualifierOpt, name, params, body)
        }
      }
    }

  lazy val intLiteral: P[Ps[AST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.IntLiteral(num)) =>
        Right(Ps(AST.Expression.IntLiteral(num))(using tok.sourceLocation))
      case tok =>
        Left(ParserError.ExpectedAbstract(category = "integer literal", actualTok = tok))
    }

  lazy val stringLiteral: P[Ps[AST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.StringLiteral(str)) =>
        Right(Ps(AST.Expression.StringLiteral(str))(using tok.sourceLocation))
      case tok =>
        Left(ParserError.ExpectedAbstract(category = "string literal", actualTok = tok))
    }

  lazy val setConstructorExpr: P[Ps[AST.Expression.SetConstructor]] =
    ps {
      braces(commaSep(expression)).map(AST.Expression.SetConstructor(_))
    }

  lazy val opCallExpr: P[Ps[AST.Expression.OpCall]] =
    ps {
      (path ~? opt(parentheses(comma1Sep(expression)))).map {
        case name ~ arguments => AST.Expression.OpCall(name, arguments.getOrElse(Nil))
      }
    }
  
  lazy val expression: P[Ps[AST.Expression]] =
    lzy {
      val binOp = ps {
        (expression ~ ps(BinaryOperator.values.iterator.map(op).reduce(_ | _)) ~ expression).map {
          case lhs ~ op ~ rhs => AST.Expression.BinaryOpCall(op.map(_.operator), lhs, rhs)
        }
      }

      binOp
      | intLiteral
      | stringLiteral
      | opCallExpr
      | setConstructorExpr
      | parentheses(expression)
    }

  lazy val await: P[Ps[AST.Statement.Await]] =
    ps {
      kw(Keyword.`await`) ~> expression.map(AST.Statement.Await(_))
    }

  lazy val path: P[Ps[AST.Path]] =
    ps(rep1sep(ps(name), pn(Punctuation.`.`)).map(parts => AST.Path(parts.toNonEmptyList)))

  lazy val assignLhs: P[Ps[AST.AssignLhs]] =
    ps {
      lzy {
        ps(name).map(AST.AssignLhs.Name(_))
        | (lzy(assignLhs) ~ brackets(expression)).map { case prefix ~ index => AST.AssignLhs.Index(prefix, index) }
      }
    }

  lazy val assignPair: P[Ps[AST.AssignPair]] =
    ps {
      (assignLhs ~? (pn(Punctuation.`:=`) ~> binding)).map {
        case lhs ~ rhs => AST.AssignPair(lhs, rhs)
      }
    }

  lazy val assignment: P[Ps[AST.Statement.Assignment]] =
    ps {
      rep1sep(assignPair, pn(Punctuation.`||`)).map(pairs => AST.Statement.Assignment(pairs.toNonEmptyList))
    }

  lazy val instanceDecl: P[Ps[AST.InstanceDecl]] =
    ps {
      (path ~ parentheses(commaSep(callArg))).map {
        case path ~ args => AST.InstanceDecl(path, args)
      }
    }

  lazy val callArg: P[Ps[AST.CallArg]] =
    ps {
      lzy {
        expression.map(AST.CallArg.Expr(_))
        | (kw(Keyword.`impl`) ~> path.map(AST.CallArg.Alias(_)))
        | (kw(Keyword.`instance`) ~> instanceDecl.map(AST.CallArg.Instance(_)))
      }
    }

  lazy val call: P[Ps[AST.Statement.Call]] =
    ps {
      (kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(callArg, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`))).map {
        case path ~ arguments =>
          AST.Statement.Call(path, arguments.toList)
      }
    }

  lazy val binding: P[Ps[AST.Binding]] =
    lzy {
      lazy val callOrExpr: P[AST.Binding] =
        callStmt.map(AST.Binding.Call(_))
        | expression.map(AST.Binding.Value(_))

      lazy val inner: P[Ps[AST.Binding]] =
        ps(op(BinaryOperator.`\\in`) ~> lzy(inner).map(AST.Binding.Selection(_)))
        | ps(callOrExpr)
      
      ps(op(BinaryOperator.`=`) ~> callOrExpr)
      | ps(op(BinaryOperator.`\\in`) ~> inner.map(AST.Binding.Selection(_)))
    }

  lazy val letStmt: P[Ps[AST.Statement.Let]] =
    ps {
      (kw(Keyword.`let`) ~> ps(name) ~ binding).map {
        case name ~ binding => AST.Statement.Let(name, binding)
      }
    }

  lazy val varStmt: P[Ps[AST.Statement.Var]] =
    ps {
      (kw(Keyword.`var`) ~> ps(name) ~ binding).map {
        case name ~ binding => AST.Statement.Var(name, binding)
      }
    }

  lazy val ifStmt: P[Ps[AST.Statement.If]] =
    ps {
      ((kw(Keyword.`if`) ~> pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))
      ~ block
      ~ opt(kw(Keyword.`else`) ~> block))
      .map {
        case condition ~ thenBlock ~ elseBlockOpt =>
          AST.Statement.If(condition, thenBlock, elseBlockOpt)
      }
    }

  lazy val callStmt: P[Ps[AST.Statement.Call]] =
    ps {
      lzy {
        (kw(Keyword.`call`) ~> path ~ parentheses(commaSep(callArg))).map {
          case path ~ arguments =>
            AST.Statement.Call(path, arguments.toList)
        }
      }
    }

  lazy val statement: P[Ps[AST.Statement]] =
    lzy {
      await
      | assignment
      | letStmt
      | varStmt
      | ifStmt
      | callStmt
      | block
    }

  lazy val block: P[Ps[AST.Statement.Block]] =
    ps {
      braces(rep(statement <~ pn(Punctuation.`;`))).map(stmts => AST.Statement.Block(stmts.toList))
    }

  lazy val `import`: P[Ps[AST.Import]] =
    ps(ps(name).map(AST.Import.Name(_)))

  lazy val imports: P[List[Ps[AST.Import]]] =
    kw(Keyword.`import`) ~> comma1Sep(`import`).map(_.toList)

  lazy val module: P[Ps[AST.Module]] =
    ps {
      (kw(Keyword.`module`) ~> ps(name) ~ opt(imports) ~ rep(definition)).map {
        case name ~ importsOpt ~ definitions =>
          AST.Module(name, importsOpt.getOrElse(Nil), definitions.toList)
      }
    }

  def apply(tokens: EvalList[Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]], path: String, offsetStart: Int = 0): Either[NonEmptyList[ParserError], Ps[AST.Module]] = {
    val result = phrase(module).parse(distcompiler.parsing.TokenInput(
      tokens = tokens.map {
        case Left(tokErrs) => Left(ParserError.FromTokenizer(tokErrs))
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