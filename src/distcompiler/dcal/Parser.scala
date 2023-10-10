package distcompiler.dcal

import cats.*
import cats.syntax.all.given
import cats.data.*

import distcompiler.parsing.{Ps, SourceLocation}
import distcompiler.util.EvalList
import ParsedAST as AST

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
      | ps(statements.map(AST.CheckDirective.CheckTimeStatements(_)))
    }

  lazy val checkBlock: P[Ps[AST.CheckBlock]] =
    ps {
      blockLike(checkDirective).map(AST.CheckBlock(_))
    }

  lazy val definitionCheck: P[Ps[AST.Definition.Check]] =
    ps {
      (kw(Keyword.check) ~> ps(name) ~ checkBlock).map {
        case name ~ block =>
          AST.Definition.Check(name, block)
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
      | statements.map(AST.InterfaceMember.ConcreteStatements(_))
    }

  lazy val interfaceRef: P[Ps[AST.InterfaceRef]] =
    ps(path.map(AST.InterfaceRef(_)))

  lazy val interfaceBlock: P[Ps[AST.InterfaceBlock]] =
    ps {
      blockLike(interfaceMember).map(AST.InterfaceBlock(_))
    }

  lazy val definitionInterface: P[Ps[AST.Definition.Interface]] =
    ps {
      (kw(Keyword.`interface`) ~> ps(name) ~ opt(kw(Keyword.`extends`) ~> comma1Sep(interfaceRef)) ~ interfaceBlock).map {
        case name ~ extendsOpt ~ block =>
          AST.Definition.Interface(name, extendsOpt.fold(List.empty)(_.toList), block)
      }
    }

  lazy val implBlock: P[Ps[AST.ImplBlock]] =
    ps {
      blockLike(definition).map(AST.ImplBlock(_))
    }
  
  lazy val definitionImpl: P[Ps[AST.Definition.Impl]] =
    ps {
      (kw(Keyword.impl) ~> ps(name) ~ parentheses(commaSep(param)) ~ opt(kw(Keyword.`extends`) ~> comma1Sep(interfaceRef)) ~ implBlock).map {
        case name ~ params ~ interfaces ~ block =>
          AST.Definition.Impl(name, params, interfaces.fold(List.empty)(_.toList), block)
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
      val subst: P[Ps[AST.FunctionSubstitutionBranch]] =
        ps {
          val index: P[Ps[AST.Expression]] =
            (pn(Punctuation.`.`) ~> ps(name.map(AST.Expression.StringLiteral(_))))
            | brackets(expression)

          (ps(pn(Punctuation.!) ~>? rep1(index) ~ (pn(Punctuation.`=`) ~> expression))).map {
            case ps @ Ps(indices ~ value) =>
              AST.FunctionSubstitutionBranch(
                indices.toNonEmptyList,
                Ps(AST.FunctionSubstitutionBody(
                  Ps(AST.FunctionSubstitutionAnchor(ps.sourceLocation, value))(using ps.sourceLocation),
                ))(using ps.sourceLocation),
              )
          }
        }

      brackets_?(expression ~? (kw(Keyword.EXCEPT) ~> rep1(subst))).map {
        case function ~ substitutions => AST.Expression.FunctionSubstitution(function, substitutions.toNonEmptyList)
      }
    }

  lazy val expression: P[Ps[AST.Expression]] =
    lzy {
      precedenceTree[Precedence, Ps[AST.Expression]]
        .levelAssoc(Precedence(16, 16)) { rec => _ =>
          ps {
            (rec ~? brackets(expression)).map {
              case function ~ argument => AST.Expression.FunctionApplication(function, argument)
            }
          }
        }
        .levelAssoc(Precedence.max) { rec => _ =>
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
        .levelsAssoc {
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

  lazy val await: P[Ps[AST.SingleStatement.Await]] =
    ps {
      kw(Keyword.`await`) ~> expression.map(AST.SingleStatement.Await(_))
    }

  lazy val pathSegment: P[Ps[AST.PathSegment]] =
    ps(name).map(AST.PathSegment.fromName)
    // | anyTok
    //     .constrain {
    //       case ps @ Ps(Token.Punctuation(op: BinaryOperator)) => Right(ps.as(op))
    //       case actualTok => Left(ParserError.ExpectedAbstract("binary operator", actualTok))
    //     }
    //     .map(AST.PathSegment.fromPunctuation)

  lazy val path: P[Ps[AST.Path]] =
    ps(rep1sep(pathSegment, pn(Punctuation.`.`)).map(parts => AST.Path(parts.toNonEmptyList)))

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

  lazy val assignment: P[Ps[AST.SingleStatement.Assignment]] =
    ps {
      rep1sep(assignPair, pn(Punctuation.`||`)).map(pairs => AST.SingleStatement.Assignment(pairs.toNonEmptyList))
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

  lazy val call: P[Ps[AST.SingleStatement.Call]] =
    ps {
      (kw(Keyword.`call`) ~> path ~ (pn(Punctuation.`(`) ~> repsep(callArg, pn(Punctuation.`,`)) <~ pn(Punctuation.`)`))).map {
        case path ~ arguments =>
          AST.SingleStatement.Call(path, arguments.toList)
      }
    }

  object binding {
    private def valImpl(needEq: Boolean): P[Ps[AST.ValBinding]] =
      lzy {
        lazy val callOrExpr: P[AST.ValBinding] =
          callStmt.map(call => AST.ValBinding.Call(call.value))
          | expression.map(AST.ValBinding.Value(_))

        lazy val inner: P[Ps[AST.ValBinding]] =
          ps(pn(Punctuation.`\\in`) ~> lzy(inner).map(AST.ValBinding.Selection(_)))
          | ps(callOrExpr)
        
        (if(needEq) ps(pn(Punctuation.`=`) ~>? callOrExpr) else ps(callOrExpr))
        | ps(pn(Punctuation.`\\in`) ~> inner.map(AST.ValBinding.Selection(_)))
      }

    lazy val eqVal: P[Ps[AST.ValBinding]] = valImpl(needEq = true)

    lazy val noEqVal: P[Ps[AST.ValBinding]] = valImpl(needEq = false)

    lazy val eqInst: P[Ps[AST.Binding]] =
      ps {
        (pn(Punctuation.`=`) ~>? kw(Keyword.instance) ~> instanceDecl.map(AST.Binding.Instance(_)))
        | eqVal.map(AST.Binding.Val(_))
      }
  }

  lazy val ifStmt: P[Ps[AST.SingleStatement.If]] =
    ps {
      ((kw(Keyword.`if`) ~> pn(Punctuation.`(`) ~> expression <~ pn(Punctuation.`)`))
      ~ block
      ~ opt(kw(Keyword.`else`) ~> block))
      .map {
        case condition ~ thenBlock ~ elseBlockOpt =>
          AST.SingleStatement.If(condition, thenBlock, elseBlockOpt)
      }
    }

  lazy val callStmt: P[Ps[AST.SingleStatement.Call]] =
    ps {
      (kw(Keyword.`call`) ~> path ~ parentheses(commaSep(callArg))).map {
        case path ~ arguments =>
          AST.SingleStatement.Call(path, arguments.toList)
      }
    }

  lazy val deferStmt: P[Ps[AST.SingleStatement.Defer]] =
    ps {
      kw(Keyword.defer) ~> ps(name).map(AST.SingleStatement.Defer(_))
    }

  lazy val returnStmt: P[Ps[AST.SingleStatement.Return]] =
    ps {
      kw(Keyword.`return`) ~> binding.noEqVal.map(AST.SingleStatement.Return(_))
    }

  lazy val eitherStmt: P[Ps[AST.SingleStatement.Either]] =
    ps {
      (kw(Keyword.either) ~> block ~ rep(kw(Keyword.or) ~> block)).map {
        case firstBlock ~ restBlock =>
          AST.SingleStatement.Either(NonEmptyList(firstBlock, restBlock.toList))
      }
    }

  lazy val forkStmt: P[Ps[AST.SingleStatement.Fork]] =
    ps {
      (kw(Keyword.fork) ~> opt(parentheses(comma1Sep(quantifierBound))) ~ rep1sep(block, kw(Keyword.and))).map {
        case qbsOpt ~ blocks =>
          AST.SingleStatement.Fork(qbsOpt.fold(List.empty)(_.toList), blocks.toNonEmptyList)
      }
    }

  lazy val spawnStmt: P[Ps[AST.SingleStatement.Spawn]] =
    ps {
      (kw(Keyword.spawn) ~> opt(parentheses(comma1Sep(quantifierBound))) ~ block).map {
        case qbsOpt ~ block =>
          AST.SingleStatement.Spawn(qbsOpt.fold(List.empty)(_.toList), block)
      }
    }

  lazy val assertStmt: P[Ps[AST.SingleStatement.Assert]] =
    ps {
      (kw(Keyword.assert) ~> expression).map(AST.SingleStatement.Assert(_))
    }

  lazy val assumeStmt: P[Ps[AST.SingleStatement.Assume]] =
    ps {
      (kw(Keyword.assume) ~> expression).map(AST.SingleStatement.Assume(_))
    }

  private lazy val restStmts: P[Option[Ps[AST.Statements]]] =
    opt(pn(Punctuation.`;`)) ~>? opt(statements) // allow trailing semicolon

  lazy val letStmt: P[Ps[AST.Statements.Let]] =
    ps {
      (kw(Keyword.`let`) ~> ps(name) ~ binding.eqInst ~ restStmts).map {
        case name ~ binding ~ restOpt => AST.Statements.Let(name, binding, restOpt)
      }
    }

  lazy val varStmt: P[Ps[AST.Statements.Var]] =
    ps {
      (kw(Keyword.`var`) ~> ps(name) ~ binding.eqVal ~ restStmts).map {
        case name ~ binding ~ restOpt => AST.Statements.Var(name, binding, restOpt)
      }
    }

  lazy val statements: P[Ps[AST.Statements]] =
    lzy {
      letStmt
      | varStmt
      | ps((singleStatement ~ restStmts).map { case firstStmt ~ restOpt => AST.Statements.Single(firstStmt, restOpt) })
    }

  lazy val singleStatement: P[Ps[AST.SingleStatement]] =
    lzy {
      await
      | assignment
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
      | ps(definition.map(AST.SingleStatement.LocalDefinition(_)))
    }

  lazy val block: P[Ps[AST.SingleStatement.Block]] =
    ps(braces(opt(statements)).map(AST.SingleStatement.Block(_)))

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