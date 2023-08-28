package test.distcompiler.dcal

import cats.*
import cats.data.Chain
import cats.syntax.all.given

// import distcompiler.util.SummonTuple.given // idk why this is necessary

import izumi.reflect.Tag

import distcompiler.util.EvalList
import distcompiler.dcal.{AST, Tokenizer, Parser, Token, Punctuation, Keyword, BinaryOperator}
import distcompiler.parsing.{Ps, SourceLocation}
import distcompiler.transform.*
import distcompiler.transform.Generatable.given

class ParserTests extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(1, scala.concurrent.duration.HOURS)

  import AST.*
  import Generator.*

  given genPs[T: Tag](using gen: Generatable[T]): Generatable[Ps[T]] = gen.map(Ps(_))

  given Generatable[BinaryOperator] = Generatable.derived

  given Generatable[AST.AssignLhs] = Generatable.derived
  given Generatable[AST.Expression] = Generatable.derived
  given Generatable[AST.CallArg] = Generatable.derived
  given Generatable[AST.Statement] = Generatable.derived
  given Generatable[AST.Binding] = Generatable.derived
  given Generatable[AST.CheckDirective] = Generatable.derived
  given Generatable[AST.Definition.Qualifier] = Generatable.derived
  given Generatable[AST.Definition] = Generatable.derived
  given Generatable[AST.InstanceDecl] = Generatable.derived
  given Generatable[AST.Param] = Generatable.derived
  given Generatable[AST.Path] = Generatable.derived
  given Generatable[AST.QuantifierBound] = Generatable.derived
  given Generatable[AST.Module] = Generatable.derived
  given Generatable[AST.AssignPair] = Generatable.derived
  given Generatable[AST.InterfaceMember] = Generatable.derived
  given Generatable[AST.Import] = Generatable.derived

  private given dummyLoc: SourceLocation = SourceLocation("dummy", offsetStart = -1, offsetEnd = -1)

  // if we focus on generating lists that are too long we'll never get very deep. length <= 3 seems good here.

  private type GC[T] = Generator[Chain[T]]

  private def tok(tok: Token): GC[Token] =
    pure(Chain.one(tok))

  private def toks(toks: Token*): GC[Token] =
    pure(Chain.fromSeq(toks))

  private def renderSepBy[F[_]: Foldable, T, U](seq: F[T])(sep: Chain[U])(fn: T => Generator[Chain[U]]): GC[U] =
    seq.foldMap(Chain.one)
      .traverse[Generator, Chain[Chain[U]]](elem => fn(elem).map(Chain.one))
      .map(_.intercalate(Chain.one(sep)).flatten)

  private def renderCommaSep[F[_]: Foldable, T](seq: F[T])(fn: T => GC[Token]): GC[Token] =
    renderSepBy(seq)(Chain.one(Token.Punctuation(Punctuation.`,`)))(fn)

  private def renderImports(imports: List[Ps[Import]]): GC[Token] =
    imports match {
      case Nil => pure(Chain.nil)
      case imports =>
        tok(Token.Keyword(Keyword.`import`))
        ++ renderCommaSep(imports) {
          case Ps(Import.Name(name)) => pure(Chain.one(Token.Name(name.value)))
        }
    }

  private def renderPath(path: Path): GC[Token] = {
    val Path(parts) = path
    renderSepBy(parts)(Chain.one(Token.Punctuation(Punctuation.`.`)))(ps => tok(Token.Name(ps.value)))
  }

  private def renderExpression(expression: Expression, needGroup: Boolean = false): GC[Token] = {
    def addGroup(body: GC[Token]): GC[Token] =
      tok(Token.Punctuation(Punctuation.`(`))
      ++ body
      ++ tok(Token.Punctuation(Punctuation.`)`))

    def groupOrNo(body: GC[Token]): GC[Token] =
      if(needGroup) {
        addGroup(body)
      } else {
        body | addGroup(body)
      }

    groupOrNo {
      expression match {
        case Expression.IntLiteral(value) =>
          tok(Token.IntLiteral(value))
        case Expression.StringLiteral(value) =>
          tok(Token.StringLiteral(value))
        case Expression.OpCall(path, arguments) =>
            renderPath(path.value)
            ++ (if(arguments.nonEmpty) {
              tok(Token.Punctuation(Punctuation.`(`))
              ++ renderCommaSep(arguments)(expr => renderExpression(expr.value))
              ++ tok(Token.Punctuation(Punctuation.`)`))
            } else pure(Chain.empty))
        case Expression.BinaryOpCall(op, lhs, rhs) =>
          renderExpression(lhs.value, needGroup = true)
          ++ tok(Token.BinaryOperator(op.value))
          ++ renderExpression(rhs.value, needGroup = true)
        case Expression.SetConstructor(members) =>
          tok(Token.Punctuation(Punctuation.`{`))
          ++ renderCommaSep(members)(expr => renderExpression(expr.value))
          ++ tok(Token.Punctuation(Punctuation.`}`))
        case Expression.Choose(binding, body) => ???
        case Expression.Exists(bindings, body) => ???
        case Expression.Forall(bindings, body) => ???
        case Expression.FunctionCall(function, argument) => ???
        case Expression.FunctionConstructor(bindings, body) => ???
        case Expression.FunctionSubstitution(function, substitutions) => ???
        case Expression.RecordConstructor(fields) => ???
        case Expression.RecordSet(fields) => ???
        case Expression.SetComprehension(body, binds) => ???
        case Expression.SetRefinement(binding, body) => ???
        case Expression.SubstitutionPoint(expr, name) => ???
        case Expression.TupleConstructor(elements) => ???
      }
    }
  }

  private def renderBinding(binding: Binding, needEquals: Boolean = true): GC[Token] =
    binding match {
      case Binding.Value(expr) => 
        (if(needEquals) {
          tok(Token.BinaryOperator(BinaryOperator.`=`))
        } else {
          pure(Chain.empty)
        })
        ++ renderExpression(expr.value)
      case Binding.Selection(binding) =>
        tok(Token.BinaryOperator(BinaryOperator.`\\in`))
        ++ renderBinding(binding.value, needEquals = false)
      case Binding.Call(call) =>
        (if(needEquals) {
          tok(Token.BinaryOperator(BinaryOperator.`=`))
        } else {
          pure(Chain.empty)
        })
        ++ renderStatement(call.value)
    }

  private def renderAssignLhs(assignLhs: AssignLhs): GC[Token] =
    ???

  private def renderCallArg(callArg: CallArg): GC[Token] =
    ???

  private def renderStatement(statement: Statement): GC[Token] =
    statement match {
      case Statement.Await(expression) =>
        tok(Token.Keyword(Keyword.`await`))
        ++ renderExpression(expression.value)
      case Statement.Assignment(pairs) =>
        renderSepBy(pairs)(Chain.one(Token.Punctuation(Punctuation.`||`))) {
          case Ps(AssignPair(lhs, binding)) =>
            renderAssignLhs(lhs.value)
            ++ (binding.value match {
              case needsEq @ (_: Binding.Value | _: Binding.Call) => 
                tok(Token.Punctuation(Punctuation.`:=`))
                ++ renderBinding(needsEq, needEquals = false)
              case noEq =>
                renderBinding(noEq, needEquals = false)
            })
        }
      case Statement.Let(name, binding) =>
        tok(Token.Keyword(Keyword.`let`))
        ++ tok(Token.Name(name.value))
        ++ renderBinding(binding.value)
      case Statement.Var(name, binding) =>
        tok(Token.Keyword(Keyword.`var`))
        ++ tok(Token.Name(name.value))
        ++ renderBinding(binding.value)
      case Statement.Block(statements) =>
        tok(Token.Punctuation(Punctuation.`{`))
        ++ Chain.fromSeq(statements)
          .flatTraverse(stmt => renderStatement(stmt.value))
        ++ tok(Token.Punctuation(Punctuation.`}`))
      case Statement.If(predicate, thenBlock, elseBlockOpt) =>
        tok(Token.Keyword(Keyword.`if`))
        ++ tok(Token.Punctuation(Punctuation.`(`))
        ++ renderExpression(predicate.value)
        ++ tok(Token.Punctuation(Punctuation.`)`))
        ++ renderStatement(thenBlock.value)
        ++ (elseBlockOpt match {
          case None => pure(Chain.empty)
          case Some(Ps(elseBlock)) =>
            tok(Token.Keyword(Keyword.`else`))
            ++ renderStatement(elseBlock)
        })
      case Statement.Call(path, arguments) =>
        tok(Token.Keyword(Keyword.`call`))
        ++ renderPath(path.value)
        ++ tok(Token.Punctuation(Punctuation.`(`))
        ++ renderCommaSep(arguments)(arg => renderCallArg(arg.value))
        ++ tok(Token.Punctuation(Punctuation.`)`))
      case Statement.Assert(property) => ???
      case Statement.Assume(property) => ???
      case Statement.Defer(label) => ???
      case Statement.Either(blocks) => ???
      case Statement.Fork(bindings, blocks) => ???
      case Statement.Instance(name, decl) => ???
      case Statement.LocalDefinition(defn) => ???
      case Statement.Return(value) => ???
      case Statement.Spawn(bindings, statement) => ???
    }

  private def renderParam(param: Param): GC[Token] =
    ???

  private def renderDefinition(definition: Definition): GC[Token] =
    ???

  // case Ps(Definition(name, params, body)) =>
  //         tok(Token.Keyword(Keyword.`def`))
  //         ++ tok(Token.Name(name.value))
  //         ++ tok(Token.Punctuation(Punctuation.`(`))
  //         ++ renderCommaSep(params)(renderDefParam)
  //         ++ tok(Token.Punctuation(Punctuation.`)`))
  //         ++ renderStatement(body.value)

  private def renderDefinitions(definitions: List[Ps[Definition]]): GC[Token] =
    Chain.fromIterableOnce(definitions)
      .flatTraverse[Generator, Token](ps => renderDefinition(ps.value))

  private def renderModule(module: Module): GC[Token] = {
    val Module(name, imports, definitions) = module

    tok(Token.Keyword(Keyword.`module`))
    ++ tok(Token.Name(name.value))
    ++ renderImports(imports)
    ++ renderDefinitions(definitions)
  }

  def toTokensAndBack(genModule: Generator[Module])(applyConstraints: (checker: Checker[(Module, List[Token])]) => checker.Self): Unit = {
    applyConstraints {
      genModule
        .andThen { module =>
          // get all possible token renderings of the generated module
          pure(module).product(renderModule(module).force_!.map(_.toList))
        }
        .toChecker
        .withPrintExamples(printExamples = false)
    }
      .forall {
        case (expectedModule, tokens) =>
          val result = Parser(
            EvalList.fromIterableOnce(
              tokens
                .iterator
                .map(Ps(_))
                .map(Right(_))),
            path = "<dummy>",
          )
              
          assertEquals(result, Right(Ps(expectedModule)))
      }
      .run()
  }

  // note: use single token strings / bigints because the parser is insensitive to the values

  test("to tokens and back (fully general)") {
    toTokensAndBack {
      Generatable[Module]
        .build
        .replace[String](pure("str"))
        .replace[BigInt](pure(BigInt(0)))
        .apply
    } { checker =>
      checker
        .exists {
          case (_, tokens) => tokens.size >= 50
        }
    }
  }
  
  test("to tokens and back (focus on one definition)") {
    toTokensAndBack {
      Generatable[Module]
        .build
        .replace[BigInt](pure(BigInt(0)))
        .replace[String](pure("str"))
        .replace[List[Ps[Definition]]] {
          listOf(Generatable[Ps[Definition]].any, limit = 1)
        }
        .replace[List[Ps[Param]]] {
          listOf(Generatable[Ps[Param]].any, limit = 2)
        }
        .apply
    } { checker =>
      checker
        .exists {
          case (module, _) =>
            // make sure we generate at least one level of nesting for exprs (same for stmts below)
            Transformable[Module]
              .combining[Count]
              .incrementAt[Ps[Expression]](_ => true)
              .apply(module)
              .depth >= 2
        }
        .exists {
          case (module, _) =>
            Transformable[Module]
              .combining[Count]
              .incrementAt[Ps[Statement]](_ => true)
              .apply(module)
              .depth >= 2
        }
    }
  }
}
