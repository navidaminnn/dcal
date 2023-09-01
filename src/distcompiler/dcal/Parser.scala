package distcompiler.dcal

import cats.*
import cats.syntax.all.given
import cats.data.*

import distcompiler.parsing.{Ps, SourceLocation}
import distcompiler.util.EvalList
import AST.*

object Parser {
  type Elem = Ps[Token]
  type Error = ParserError
  type Input = distcompiler.parsing.TokenInput[Elem, Error]
  given errorOps: distcompiler.parsing.ErrorOps[Elem, Error] with {
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

  def ps[T](parser: P[T]): P[Ps[T]] =
    capturingPosition(parser).map((v, pos) => Ps(v)(using pos))

  def parentheses[T](parser: P[T]): P[T] =
    pn(Punctuation.`(`) ~> parser <~ pn(Punctuation.`)`)

  def brackets[T](parser: P[T]): P[T] =
    pn(Punctuation.`[`) ~> parser <~ pn(Punctuation.`]`)

  def brackets_?[T](parser: P[T]): P[T] =
    pn(Punctuation.`[`) ~>? parser <~ pn(Punctuation.`]`)

  def braces[T](parser: P[T]): P[T] =
    pn(Punctuation.`{`) ~> parser <~ pn(Punctuation.`}`)

  def braces_?[T](parser: P[T]): P[T] =
    pn(Punctuation.`{`) ~>? parser <~ pn(Punctuation.`}`)

  def commaSep[T](elem: P[T]): P[List[T]] =
    repsep(elem, pn(Punctuation.`,`)).map(_.toList)

  def comma1Sep[T](elem: P[T]): P[NonEmptyList[T]] =
    rep1sep(elem, pn(Punctuation.`,`)).map(_.toNonEmptyList)

  def blockLike[T](elem: P[T]): P[List[T]] =
    braces(rep(elem <~ opt(pn(Punctuation.`;`))).map(_.toList))

  lazy val name: P[String] =
    anyTok.constrain {
      case Ps(Token.Name(name)) => Right(name)
      case tok => Left(ParserError.ExpectedAbstract(category = "name", actualTok = tok))
    }

  lazy val param: P[Ps[AST.Param]] =
    ps {
      (opt(ps(name)) ~? (kw(Keyword.`impl`) ~> path)).map {
        case aliasOpt ~ interface => AST.Param.Impl(aliasOpt, interface)
      }
      | ps(name).map(AST.Param.Name.apply)
    }

  lazy val qualifier: P[Ps[AST.Definition.Qualifier]] =
    ps {
      kw(Keyword.`pure`).as(AST.Definition.Qualifier.Pure)
      | kw(Keyword.`async`).as(AST.Definition.Qualifier.Async)
    }

  lazy val definitionDef: P[Ps[AST.Definition.Def]] =
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

  lazy val checkDirective: P[Ps[AST.CheckDirective]] =
    lzy {
      ps {
        kw(Keyword.`override`) ~>? (
          (kw(Keyword.`let`) ~> path ~ binding.eqInst).map {
            case path ~ binding => AST.CheckDirective.OverrideLet(path, binding)
          }
          | (kw(Keyword.`var`) ~> path ~ (pn(Punctuation.`=`) ~> expression)).map {
            case path ~ expression => AST.CheckDirective.OverrideVar(path, expression)
          }
        )
      }
      | ps(statement.map(AST.CheckDirective.CheckTimeStatement(_)))
    }

  lazy val definitionCheck: P[Ps[AST.Definition.Check]] =
    ps {
      (kw(Keyword.check) ~> ps(name) ~ blockLike(checkDirective)).map {
        case name ~ directives =>
          AST.Definition.Check(name, directives)
      }
    }

  lazy val interfaceMember: P[Ps[AST.InterfaceMember]] =
    ps {
      (kw(Keyword.`abstract`) ~>? (
        (kw(Keyword.`let`) ~> ps(name)).map(AST.InterfaceMember.AbstractLet(_))
        | (kw(Keyword.`var`) ~> ps(name)).map(AST.InterfaceMember.AbstractVar(_))
        | (opt(qualifier) ~? (kw(Keyword.`def`) ~> ps(name)) ~ parentheses(commaSep(param))).map {
          case qualifierOpt ~ name ~ params =>
            AST.InterfaceMember.AbstractDef(qualifierOpt, name, params.toList)
        }
      ))
      | statement.map(AST.InterfaceMember.ConcreteStatement(_))
    }

  lazy val definitionInterface: P[Ps[AST.Definition.Interface]] =
    ps {
      (kw(Keyword.`interface`) ~> ps(name) ~ opt(kw(Keyword.`extends`) ~> comma1Sep(path)) ~ blockLike(interfaceMember)).map {
        case name ~ extendsOpt ~ members =>
          AST.Definition.Interface(name, extendsOpt.fold(List.empty)(_.toList), members.toList)
      }
    }
  
  lazy val definitionImpl: P[Ps[AST.Definition.Impl]] =
    ps {
      (kw(Keyword.impl) ~> ps(name) ~ parentheses(commaSep(param)) ~ opt(kw(Keyword.`extends`) ~> comma1Sep(path)) ~ blockLike(definition)).map {
        case name ~ params ~ interfaces ~ definitions =>
          AST.Definition.Impl(name, params, interfaces.fold(List.empty)(_.toList), definitions.toList)
      }
    }

  lazy val definition: P[Ps[AST.Definition]] =
    lzy {
      definitionDef
      | definitionCheck
      | definitionInterface
      | definitionImpl
    }

  lazy val quantifierBound: P[Ps[AST.QuantifierBound]] =
    ps {
      ((pn(Punctuation.`<<`) ~>? comma1Sep(ps(name)) <~? pn(Punctuation.`>>`)) ~? (pn(Punctuation.`\\in`) ~> expression)).map {
        case names ~ setExpr => AST.QuantifierBound.Tuple(names, setExpr)
      }
      | (ps(name) ~ (pn(Punctuation.`\\in`) ~> expression)).map {
        case name ~ setExpr => AST.QuantifierBound.Single(name, setExpr)
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
      braces_?(opt(expression ~? opt(pn(Punctuation.`,`) ~>commaSep(expression))))
        .map(_.fold(Nil) {
          case first ~ restOpt =>
            first :: restOpt.getOrElse(Nil)
        })
        .map(AST.Expression.SetConstructor(_))
    }

  lazy val setRefinementExpr: P[Ps[AST.Expression.SetRefinement]] =
    ps {
      braces_?(quantifierBound ~? (pn(Punctuation.`:`) ~> expression)).map {
        case binding ~ body => AST.Expression.SetRefinement(binding, body)
      }
    }

  lazy val setComprehensionExpr: P[Ps[AST.Expression.SetComprehension]] =
    ps {
      braces_?(expression ~? (pn(Punctuation.`:`) ~> comma1Sep(quantifierBound))).map {
        case body ~ bindings => AST.Expression.SetComprehension(body, bindings)
      }
    }

  lazy val opCallExpr: P[Ps[AST.Expression.OpCall]] =
    ps {
      (path ~? opt(parentheses(comma1Sep(expression)))).map {
        case name ~ arguments => AST.Expression.OpCall(name, arguments.fold(List.empty)(_.toList))
      }
    }

  lazy val tupleConstructorExpr: P[Ps[AST.Expression.TupleConstructor]] =
    ps {
      (pn(Punctuation.`<<`) ~>? commaSep(expression) <~? pn(Punctuation.`>>`))
        .map(AST.Expression.TupleConstructor(_))
    }

  lazy val recordConstructorExpr: P[Ps[AST.Expression.RecordConstructor]] =
    ps {
      brackets_?(comma1Sep((ps(name) ~? (pn(Punctuation.`|->`) ~> expression)).map { case name ~ expr => (name, expr) }))
        .map(AST.Expression.RecordConstructor(_))
    }

  lazy val recordSetExpr: P[Ps[AST.Expression.RecordSet]] =
    ps {
      brackets_?(comma1Sep((ps(name) ~? (pn(Punctuation.`:`) ~> expression)).map { case name ~ expr => (name, expr) }))
        .map(AST.Expression.RecordSet(_))
    }

  lazy val functionConstructorExpr: P[Ps[AST.Expression.FunctionConstructor]] =
    ps {
      brackets_?(comma1Sep(quantifierBound) ~? (pn(Punctuation.`|->`) ~> expression )).map {
        case bindings ~ body => AST.Expression.FunctionConstructor(bindings, body)
      }
    }

  lazy val functionSubstitutionExpr: P[Ps[AST.Expression.FunctionSubstitution]] =
    ps {
      val subst: P[Ps[(NonEmptyList[Ps[Expression]], Ps[Expression])]] =
        ps {
          val index: P[Ps[Expression]] =
            (pn(Punctuation.`.`) ~> ps(name.map(AST.Expression.StringLiteral(_))))
            | brackets(expression)

          (pn(Punctuation.!) ~>? rep1(index) ~ (pn(Punctuation.`=`) ~> expression)).map {
            case indices ~ value => (indices.toNonEmptyList, value)
          }
        }

      brackets_?(expression ~? (kw(Keyword.EXCEPT) ~> rep1(subst))).map {
        case function ~ substitutions => AST.Expression.FunctionSubstitution(function, substitutions.toNonEmptyList)
      }
    }

  lazy val expression: P[Ps[AST.Expression]] =
    lzy {
      precedenceTree[Precedence, Ps[AST.Expression]]
        .levelRec(Precedence(16, 16)) { rec => _ =>
          ps {
            (rec ~? brackets(expression)).map {
              case function ~ argument => AST.Expression.FunctionApplication(function, argument)
            }
          }
        }
        .levelRec(Precedence.max) { rec => _ =>
          ps {
            (pn(Punctuation.@!) ~> ps(name) ~ rec).map {
              case name ~ expression => AST.Expression.SubstitutionPoint(name, expression)
            }
          }
        }
        .levels {
          Punctuation
            .values
            .iterator
            .collect { case op: BinaryOperator if !op.leftAssociative => op }
            .map { op =>
              op.precedence -> { lower =>
                ps {
                  (lower ~? ps(pn(op).as(op)) ~ lower).map {
                    case lhs ~ op ~ rhs => AST.Expression.BinaryOpCall(op, lhs, rhs)
                  }
                }
              }
            }
        }
        .levelsRec {
          Punctuation
            .values
            .iterator
            .collect { case op: BinaryOperator if op.leftAssociative => op }
            .map { op =>
              op.precedence -> { rec => lower =>
                ps {
                  (rec ~? ps(pn(op).as(op)) ~ lower).map {
                    case lhs ~ op ~ rhs => AST.Expression.BinaryOpCall(op, lhs, rhs)
                  }
                }
              }
            }
        }
        .bottomLevel {
          intLiteral
          | stringLiteral
          | opCallExpr
          | setConstructorExpr
          | setRefinementExpr
          | setComprehensionExpr
          | recordConstructorExpr
          | recordSetExpr
          | functionConstructorExpr
          | functionSubstitutionExpr
          | tupleConstructorExpr
          | parentheses(expression)
        }
        .parser
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
      (assignLhs ~? (pn(Punctuation.`:=`) ~> binding.noEqVal)).map {
        case lhs ~ rhs => AST.AssignPair(lhs, rhs)
      }
    }

  lazy val assignment: P[Ps[AST.Statement.Assignment]] =
    ps {
      rep1sep(assignPair, pn(Punctuation.`||`)).map(pairs => AST.Statement.Assignment(pairs.toNonEmptyList))
    }

  lazy val instanceDecl: P[Ps[AST.InstanceDecl]] =
    lzy {
      ps {
        (path ~ parentheses(commaSep(callArg))).map {
          case path ~ args => AST.InstanceDecl(path, args)
        }
      }
    }

  lazy val callArg: P[Ps[AST.CallArg]] =
    ps {
      expression.map(AST.CallArg.Expr(_))
      | (kw(Keyword.`impl`) ~> path.map(AST.CallArg.Alias(_)))
      | (kw(Keyword.`instance`) ~> instanceDecl.map(AST.CallArg.Instance(_)))
    }

  lazy val call: P[Ps[AST.Statement.Call]] =
    ps {
      (kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(callArg, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`))).map {
        case path ~ arguments =>
          AST.Statement.Call(path, arguments.toList)
      }
    }

  object binding {
    private def valImpl(needEq: Boolean): P[Ps[AST.ValBinding]] =
      lzy {
        lazy val callOrExpr: P[AST.ValBinding] =
          callStmt.map(AST.ValBinding.Call(_))
          | expression.map(AST.ValBinding.Value(_))

        lazy val inner: P[Ps[AST.ValBinding]] =
          ps(pn(Punctuation.`\\in`) ~> lzy(inner).map(AST.ValBinding.Selection(_)))
          | ps(callOrExpr)
        
        (if(needEq) ps(pn(Punctuation.`=`) ~> callOrExpr) else ps(callOrExpr))
        | ps(pn(Punctuation.`\\in`) ~> inner.map(AST.ValBinding.Selection(_)))
      }

    lazy val eqVal: P[Ps[AST.ValBinding]] = valImpl(needEq = true)

    lazy val noEqVal: P[Ps[AST.ValBinding]] = valImpl(needEq = false)

    lazy val eqInst: P[Ps[AST.Binding]] =
      ps {
        (pn(Punctuation.`=`) ~> kw(Keyword.instance) ~> instanceDecl.map(AST.Binding.Instance(_)))
        | eqVal.map(AST.Binding.Val(_))
      }
  }

  lazy val letStmt: P[Ps[AST.Statement.Let]] =
    ps {
      (kw(Keyword.`let`) ~> ps(name) ~ binding.eqInst).map {
        case name ~ binding => AST.Statement.Let(name, binding)
      }
    }

  lazy val varStmt: P[Ps[AST.Statement.Var]] =
    ps {
      (kw(Keyword.`var`) ~> ps(name) ~ binding.eqVal).map {
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

  lazy val deferStmt: P[Ps[AST.Statement.Defer]] =
    ps {
      kw(Keyword.defer) ~> ps(name).map(AST.Statement.Defer(_))
    }

  lazy val returnStmt: P[Ps[AST.Statement.Return]] =
    ps {
      kw(Keyword.`return`) ~> binding.noEqVal.map(AST.Statement.Return(_))
    }

  lazy val eitherStmt: P[Ps[AST.Statement.Either]] =
    ps {
      (kw(Keyword.either) ~> block ~ rep(kw(Keyword.or) ~> block)).map {
        case firstBlock ~ restBlock =>
          AST.Statement.Either(NonEmptyList(firstBlock, restBlock.toList))
      }
    }

  lazy val forkStmt: P[Ps[AST.Statement.Fork]] =
    ps {
      (kw(Keyword.fork) ~> opt(parentheses(comma1Sep(quantifierBound))) ~ rep1sep(block, kw(Keyword.and))).map {
        case qbsOpt ~ blocks =>
          AST.Statement.Fork(qbsOpt.fold(List.empty)(_.toList), blocks.toNonEmptyList)
      }
    }

  lazy val spawnStmt: P[Ps[AST.Statement.Spawn]] =
    ps {
      (kw(Keyword.spawn) ~> opt(parentheses(comma1Sep(quantifierBound))) ~ block).map {
        case qbsOpt ~ block =>
          AST.Statement.Spawn(qbsOpt.fold(List.empty)(_.toList), block)
      }
    }

  lazy val assertStmt: P[Ps[AST.Statement.Assert]] =
    ps {
      (kw(Keyword.assert) ~> expression).map(AST.Statement.Assert(_))
    }

  lazy val assumeStmt: P[Ps[AST.Statement.Assume]] =
    ps {
      (kw(Keyword.assume) ~> expression).map(AST.Statement.Assume(_))
    }

  lazy val statement: P[Ps[AST.Statement]] =
    lzy {
      await
      | assignment
      | letStmt
      | varStmt
      | ifStmt
      | callStmt
      | deferStmt
      | returnStmt
      | eitherStmt
      | forkStmt
      | spawnStmt
      | assertStmt
      | assumeStmt
      | block
      | ps(definition.map(AST.Statement.LocalDefinition(_)))
    }

  lazy val block: P[Ps[AST.Statement.Block]] =
    ps(blockLike(statement).map(AST.Statement.Block(_)))

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