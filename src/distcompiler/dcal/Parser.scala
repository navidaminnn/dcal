package distcompiler.dcal

import cats.*
import cats.syntax.all.given
import cats.data.NonEmptyChain

import distcompiler.parsing.{Ps, SourceLocation, InputOps}
import distcompiler.util.EvalList
import AST.*

object Parser {
  type Elem = Ps[Token]
  type Error = NonEmptyChain[ParserError]
  type Input = distcompiler.parsing.TokenInput[Elem, Error]
  given distcompiler.parsing.ErrorOps[Elem, Error] with {
    override def expectedEOF(sourceLocation: SourceLocation, actualElem: Elem): Error =
      NonEmptyChain.one(ParserError.ExpectedAbstract(category = "EOF", actualTok = actualElem))

    override def unexpectedEOF(sourceLocation: SourceLocation): Error =
      NonEmptyChain.one(ParserError.UnexpectedEOF(sourceLocation))
  }
  private val ops = distcompiler.parsing.Parser.Ops[Elem, Input, Error]
  import ops.{*, given}

  extension (self: ParserError) def err: Error = NonEmptyChain.one(self)

  private val anyTok: P[Ps[Token]] = anyElem

  def kw(keyword: Keyword): P[Token.Keyword] =
    anyTok.constrain {
      case Ps(tok @ Token.Keyword(`keyword`)) => Right(tok)
      case tok => Left(ParserError.ExpectedKeyword(expectedKeyword = keyword, actualTok = tok).err)
    }

  def pn(punctuation: Punctuation): P[Token.Punctuation] =
    anyTok.constrain {
      case Ps(tok @ Token.Punctuation(`punctuation`)) => Right(tok)
      case tok => Left(ParserError.ExpectedPunctuation(expectedPunctuation = punctuation, actualTok = tok).err)
    }

  def op(operator: BinaryOperator): P[Token.BinaryOperator] =
    anyTok.constrain {
      case Ps(tok @ Token.BinaryOperator(`operator`)) => Right(tok)
      case tok => Left(ParserError.ExpectedBinaryOperator(expectedOperator = operator, actualTok = tok).err)
    }

  def ps[T](parser: P[T]): P[Ps[T]] =
    capturingPosition(parser).mapPositioned(Ps(_))

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

  def name: P[String] =
    anyTok.constrain {
      case Ps(Token.Name(name)) => Right(name)
      case tok => Left(err(ParserError.ExpectedAbstract(category = "name", actualTok = tok)))
    }

  def param: P[Ps[AST.Param]] =
    ps {
      ps(name).map(AST.Param.Name.apply)
      | (kw(Keyword.`impl`) ~> path ~ opt(ps(name))).map {
          case interface ~ aliasOpt =>  AST.Param.Impl(interface, aliasOpt)
        }
    }

  def qualifier: P[Ps[AST.Definition.Qualifier]] =
    ps {
      kw(Keyword.`pure`).as(AST.Definition.Qualifier.Pure)
      | kw(Keyword.`async`).as(AST.Definition.Qualifier.Async)
    }

  def definition: P[Ps[AST.Definition]] =
    ps {
      (opt(qualifier)
      ~ (kw(Keyword.`def`) ~> ps(name))
      ~ parentheses(commaSep(param))
      ~ block
      ).map {
        case qualifierOpt ~ name ~ params ~ body =>
          AST.Definition.Def(qualifierOpt, name, params, body)
      }
    }

  def intLiteral: P[Ps[AST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.IntLiteral(num)) =>
        Right(Ps(AST.Expression.IntLiteral(num))(using tok.sourceLocation))
      case tok =>
        Left(ParserError.ExpectedAbstract(category = "integer literal", actualTok = tok).err)
    }

  def stringLiteral: P[Ps[AST.Expression]] =
    anyTok.constrain {
      case tok @ Ps(Token.StringLiteral(str)) =>
        Right(Ps(AST.Expression.StringLiteral(str))(using tok.sourceLocation))
      case tok =>
        Left(ParserError.ExpectedAbstract(category = "string literal", actualTok = tok).err)
    }

  def setConstructorExpr: P[Ps[AST.Expression.SetConstructor]] =
    capturingPosition(pn(Punctuation.`{`) ~> repsep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`}`))
      .mapPositioned(members => Ps(AST.Expression.SetConstructor(members.toList)))

  def opCallExpr: P[Ps[AST.Expression.OpCall]] =
    capturingPosition(path ~ opt(pn(Punctuation.`(`) ~> rep1sep(expression, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case name ~ arguments =>
          Ps(AST.Expression.OpCall(name, arguments.map(_.toList).getOrElse(Nil)))
      }

  lazy val exprBase: P[Ps[AST.Expression]] =
    intLiteral
    | stringLiteral
    | opCallExpr
    | setConstructorExpr
    | parentheses(expression)

  // TODO: precedence
  lazy val binOpExpr: P[Ps[AST.Expression]] =
    capturingPosition(exprBase ~ opt(ps(BinaryOperator.values.iterator.map(op).reduce(_ | _)) ~ exprBase)).mapPositioned {
      case lhs ~ None => lhs
      case lhs ~ Some(op ~ rhs) =>
        Ps(AST.Expression.BinaryOpCall(Ps(op.value.operator)(using op.sourceLocation), lhs, rhs))
    }
  
  lazy val expression: P[Ps[AST.Expression]] =
    binOpExpr

  lazy val await: P[Ps[AST.Statement.Await]] =
    capturingPosition(kw(Keyword.`await`) ~> expression)
      .mapPositioned(expr => Ps(AST.Statement.Await(expr)))

  lazy val path: P[Ps[AST.Path]] =
    ps(rep1sep(ps(name), pn(Punctuation.`.`)).map(parts => AST.Path(parts.toNonEmptyList)))

  lazy val assignLhs: P[Ps[AST.AssignLhs]] =
    ps {
      leftRec {
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
    capturingPosition(rep1sep(assignPair, pn(Punctuation.`||`)))
      .mapPositioned(pairs => Ps(AST.Statement.Assignment(pairs.toNonEmptyList)))

  lazy val instanceDecl: P[Ps[AST.InstanceDecl]] =
    ps {
      (path ~ parentheses(commaSep(callArg))).map {
        case path ~ args => AST.InstanceDecl(path, args)
      }
    }

  lazy val callArg: P[Ps[AST.CallArg]] =
    ps {
      expression.map(AST.CallArg.Expr(_))
      | (kw(Keyword.`impl`) ~> path.map(AST.CallArg.Alias(_)))
      | (kw(Keyword.`instance`) ~> instanceDecl.map(AST.CallArg.Instance(_)))
    }

  lazy val call: P[Ps[AST.Statement.Call]] =
    capturingPosition(kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(callArg, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`)))
      .mapPositioned {
        case path ~ arguments => Ps(AST.Statement.Call(path, arguments.toList))
      }

  lazy val binding: P[Ps[AST.Binding]] = {
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
      (kw(Keyword.`call`) ~> path ~ parentheses(commaSep(callArg))).map {
        case path ~ arguments =>
          AST.Statement.Call(path, arguments.toList)
      }
    }

  lazy val statement: P[Ps[AST.Statement]] =
    await
    | assignment
    | letStmt
    | varStmt
    | ifStmt
    | callStmt
    | block

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

  def apply(tokens: EvalList[Either[NonEmptyChain[Ps[TokenizerError]], Ps[Token]]], path: String, offsetStart: Int = 0): Either[NonEmptyChain[ParserError], Ps[AST.Module]] = {
    val result = phrase(module).parse(distcompiler.parsing.TokenInput(
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