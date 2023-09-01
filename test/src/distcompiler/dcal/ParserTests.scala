package test.distcompiler.dcal

import cats.*
import cats.data.*
import cats.syntax.all.given

import izumi.reflect.Tag

import distcompiler.util.{EvalList, !!!}
import distcompiler.dcal.{AST, Parser, Token, Punctuation, BinaryOperator, Keyword, ParserError, Precedence}
import distcompiler.parsing.{Ps, SourceLocation, TokenInput, ErrorOps}
import distcompiler.transform.*
import distcompiler.transform.Generatable.given

class ParserTests extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(1, scala.concurrent.duration.HOURS)

  import AST.*
  import Generator.*

  given genPs[T: Tag](using gen: Generatable[T]): Generatable[Ps[T]] = gen.map(Ps(_))

  given Generatable[BinaryOperator] = Generatable.derived
  given Generatable[Punctuation] = Generatable.derived

  given Generatable[AST.AssignLhs] = Generatable.derived
  given Generatable[AST.Expression] = Generatable.derived
  given Generatable[AST.CallArg] = Generatable.derived
  given Generatable[AST.Statements] = Generatable.derived
  given Generatable[AST.SingleStatement] = Generatable.derived
  given Generatable[AST.Binding] = Generatable.derived
  given Generatable[AST.ValBinding] = Generatable.derived
  given Generatable[AST.CheckDirective] = Generatable.derived
  given Generatable[AST.Definition.Qualifier] = Generatable.derived
  given Generatable[AST.Definition] = Generatable.derived
  given Generatable[AST.InstanceDecl] = Generatable.derived
  given Generatable[AST.Param] = Generatable.derived
  given Generatable[AST.Path] = Generatable.derived
  given Generatable[AST.PathSegment] = Generatable.derived
  given Generatable[AST.QuantifierBound] = Generatable.derived
  given Generatable[AST.Module] = Generatable.derived
  given Generatable[AST.AssignPair] = Generatable.derived
  given Generatable[AST.InterfaceMember] = Generatable.derived
  given Generatable[AST.Import] = Generatable.derived

  private given dummyLoc: SourceLocation = SourceLocation("dummy", offsetStart = -1, offsetEnd = -1)

  private type GC[T] = Generator[Chain[T]]

  private given MonoidK[GC] with {
    def combineK[A](x: GC[A], y: GC[A]): GC[A] = x ++ y
    def empty[A]: GC[A] = pure(Chain.empty)
  }

  private def tok(tok: Token): GC[Token] =
    pure(Chain.one(tok))

  private def toks(toks: Token*): GC[Token] =
    pure(Chain.fromSeq(toks))

  private def renderParentheses(gen: GC[Token]): GC[Token] =
    tok(Token.Punctuation(Punctuation.`(`))
    ++ gen
    ++ tok(Token.Punctuation(Punctuation.`)`))

  private def renderBraces(gen: GC[Token]): GC[Token] =
    tok(Token.Punctuation(Punctuation.`{`))
    ++ gen
    ++ tok(Token.Punctuation(Punctuation.`}`))

  private def renderBrackets(gen: GC[Token]): GC[Token] =
    tok(Token.Punctuation(Punctuation.`[`))
    ++ gen
    ++ tok(Token.Punctuation(Punctuation.`]`))

  private def renderSepBy[F[_]: Foldable, T, U](seq: F[T])(sep: Chain[U])(fn: T => Generator[Chain[U]]): GC[U] =
    seq.foldMap(Chain.one)
      .traverse[Generator, Chain[Chain[U]]](elem => fn(elem).map(Chain.one))
      .map(_.intercalate(Chain.one(sep)).flatten)

  private def renderCommaSep[F[_]: Foldable, T](seq: F[T])(fn: T => GC[Token]): GC[Token] =
    renderSepBy(seq)(Chain.one(Token.Punctuation(Punctuation.`,`)))(fn)

  private def renderBlockLike[F[_]: Foldable, T](seq: F[T])(fn: T => GC[Token]): GC[Token] =
    renderBraces(seq.foldLeft(pure(Chain.empty)) { (acc, elem) =>
      acc
      ++ fn(elem)
      ++ (tok(Token.Punctuation(Punctuation.`;`)) | pure(Chain.empty))
    })

  private def renderImports(imports: List[Ps[Import]]): GC[Token] =
    imports match {
      case Nil => pure(Chain.nil)
      case imports =>
        tok(Token.Keyword(Keyword.`import`))
        ++ renderCommaSep(imports) {
          case Ps(Import.Name(name)) => pure(Chain.one(Token.Name(name.value)))
        }
    }

  private def renderPathSegment(segment: PathSegment): GC[Token] =
    segment match {
      case PathSegment.Name(name) => tok(Token.Name(name.value))
      case PathSegment.Punctuation(pn) => tok(Token.Punctuation(pn.value))
    }

  private def renderPath(path: Path): GC[Token] = {
    val Path(parts) = path
    renderSepBy(parts)(Chain.one(Token.Punctuation(Punctuation.`.`)))(ps => renderPathSegment(ps.value))
  }

  private def renderExpression(expression: Expression, outerPrecedence: Precedence = Precedence.min): GC[Token] = {
    def groupOrNo(precedenceOpt: Option[Precedence] = None)(body: GC[Token]): GC[Token] =
      precedenceOpt match {
        case None => body
        case Some(precedence) =>
          if(precedence > outerPrecedence) {
            body | renderParentheses(body)
          } else {
            renderParentheses(body)
          }
      }

    expression match {
      case Expression.BinaryOpCall(op, lhs, rhs) =>
        groupOrNo(Some(op.value.precedence)) {
          renderExpression(lhs.value, op.value.precedence)
          ++ tok(Token.Punctuation(op.value.asPunctuation))
          ++ renderExpression(rhs.value, op.value.precedence)
        }
      case Expression.FunctionApplication(function, argument) =>
        groupOrNo(Some(Precedence(16, 16))) {
          renderExpression(function.value, outerPrecedence = Precedence(16, 16))
          ++ renderBrackets(renderExpression(argument.value))
        }
      case expression =>
        groupOrNo() {
          expression match {
            case Expression.BinaryOpCall(_, _, _) => !!!
            case Expression.FunctionApplication(_, _) => !!!
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
            case Expression.SetConstructor(members) =>
              renderBraces(renderCommaSep(members)(expr => renderExpression(expr.value)))
            case Expression.Choose(binding, body) =>
              tok(Token.Keyword(Keyword.CHOOSE))
              ++ renderQuantifierBound(binding.value)
              ++ tok(Token.Punctuation(Punctuation.`:`))
              ++ renderExpression(body.value, outerPrecedence = Precedence.min)
            case Expression.Exists(bindings, body) =>
              tok(Token.Punctuation(Punctuation.`\\E`))
              ++ renderCommaSep(bindings)(qb => renderQuantifierBound(qb.value))
              ++ tok(Token.Punctuation(Punctuation.`:`))
              ++ renderExpression(body.value, outerPrecedence = Precedence.min)
            case Expression.Forall(bindings, body) =>
              tok(Token.Punctuation(Punctuation.`\\A`))
              ++ renderCommaSep(bindings)(qb => renderQuantifierBound(qb.value))
              ++ tok(Token.Punctuation(Punctuation.`:`))
              ++ renderExpression(body.value, outerPrecedence = Precedence.min)
            case Expression.FunctionConstructor(bindings, body) =>
              renderBrackets {
                renderCommaSep(bindings)(binding => renderQuantifierBound(binding.value))
                ++ tok(Token.Punctuation(Punctuation.`|->`))
                ++ renderExpression(body.value)
              }
            case Expression.FunctionSubstitution(function, substitutions) =>
              renderBrackets {
                renderExpression(function.value)
                ++ substitutions.foldLeft(pure(Chain.empty)) { (acc, sub) =>
                  acc
                  ++ (sub match {
                    case Ps(FunctionSubstitutionBranch(keys, value)) =>
                      tok(Token.Punctuation(Punctuation.`!`))
                      ++ keys.foldLeft(pure(Chain.empty)) { (acc, key) =>
                        acc
                        ++ (key match {
                          case Ps(expr @ Expression.StringLiteral(str)) =>
                            toks(Token.Punctuation(Punctuation.`.`), Token.Name(str))
                            | renderBrackets(renderExpression(expr))
                          case Ps(expr) =>
                            renderBrackets(renderExpression(expr))
                        })
                      }
                      ++ tok(Token.Punctuation(Punctuation.`=`))
                      ++ renderExpression(value.anchor.body.value)
                    })
                }
              }
            case Expression.RecordConstructor(fields) =>
              renderBrackets {
                renderCommaSep(fields) {
                  case (name, value) =>
                    tok(Token.Name(name.value))
                    ++ tok(Token.Punctuation(Punctuation.`|->`))
                    ++ renderExpression(value.value)
                }
              }
            case Expression.RecordSet(fields) =>
              renderBrackets {
                renderCommaSep(fields) {
                  case (name, value) =>
                    tok(Token.Name(name.value))
                    ++ tok(Token.Punctuation(Punctuation.`:`))
                    ++ renderExpression(value.value)
                }
              }
            case Expression.SetComprehension(body, binds) =>
              renderBraces {
                renderExpression(body.value)
                ++ tok(Token.Punctuation(Punctuation.`:`))
                ++ renderCommaSep(binds) {
                  case Ps(qb) => renderQuantifierBound(qb) 
                }
              }
            case Expression.SetRefinement(binding, body) =>
              renderBraces {
                renderQuantifierBound(binding.value)
                ++ tok(Token.Punctuation(Punctuation.`:`))
                ++ renderExpression(body.value)
              }
            case Expression.SubstitutionPoint(name, expr) =>
              tok(Token.Punctuation(Punctuation.`@!`))
              ++ tok(Token.Name(name.value))
              ++ renderExpression(expr.value, outerPrecedence = Precedence.max)
            case Expression.TupleConstructor(elements) =>
              tok(Token.Punctuation(Punctuation.`<<`))
              ++ renderCommaSep(elements)(elem => renderExpression(elem.value))
              ++ tok(Token.Punctuation(Punctuation.`>>`))
          }
        }
    }
  }

  private def renderValBinding(binding: ValBinding, needEquals: Boolean): GC[Token] =
    binding match {
      case ValBinding.Value(expr) => 
        (if(needEquals) {
          tok(Token.Punctuation(Punctuation.`=`))
        } else {
          pure(Chain.empty)
        })
        ++ renderExpression(expr.value)
      case ValBinding.Selection(binding) =>
        tok(Token.Punctuation(Punctuation.`\\in`))
        ++ renderValBinding(binding.value, needEquals = false)
      case ValBinding.Call(call) =>
        (if(needEquals) {
          tok(Token.Punctuation(Punctuation.`=`))
        } else {
          pure(Chain.empty)
        })
        ++ renderSingleStatement(call.value)
    }

  private def renderBinding(binding: Binding, needEquals: Boolean): GC[Token] =
    binding match {
      case Binding.Val(binding) => renderValBinding(binding.value, needEquals)
      case Binding.Instance(decl) =>
        (if(needEquals) tok(Token.Punctuation(Punctuation.`=`)) else pure(Chain.empty))
        ++ tok(Token.Keyword(Keyword.instance))
        ++ renderInstanceDecl(decl.value)
    }

  private def renderAssignLhs(assignLhs: AssignLhs): GC[Token] =
    assignLhs match {
      case AssignLhs.Name(name) =>
        tok(Token.Name(name.value))
      case AssignLhs.Index(prefix, index) =>
        renderAssignLhs(prefix.value)
        ++ tok(Token.Punctuation(Punctuation.`[`))
        ++ renderExpression(index.value)
        ++ tok(Token.Punctuation(Punctuation.`]`))
    }

  private def renderInstanceDecl(decl: InstanceDecl): GC[Token] = {
    val InstanceDecl(path, args) = decl

    tok(Token.Keyword(Keyword.instance))
    ++ renderPath(path.value)
    ++ renderParentheses(renderCommaSep(args)(arg => renderCallArg(arg.value)))
  }

  private def renderCallArg(callArg: CallArg): GC[Token] =
    callArg match {
      case CallArg.Expr(expression) =>
        renderExpression(expression.value)
      case CallArg.Alias(path) =>
        tok(Token.Keyword(Keyword.alias))
        ++ renderPath(path.value)
      case CallArg.Instance(decl) =>
        renderInstanceDecl(decl.value)
    }

  private def renderQuantifierBound(qb: QuantifierBound): GC[Token] =
    qb match {
      case QuantifierBound.Single(name, set) =>
        tok(Token.Name(name.value))
        ++ tok(Token.Punctuation(Punctuation.`\\in`))
        ++ renderExpression(set.value)
      case QuantifierBound.Tuple(names, set) =>
        tok(Token.Punctuation(Punctuation.`<<`))
        ++ renderCommaSep(names)(name => tok(Token.Name(name.value)))
        ++ tok(Token.Punctuation(Punctuation.`>>`))
        ++ tok(Token.Punctuation(Punctuation.`\\in`))
        ++ renderExpression(set.value)
    }

  private def renderStatements(statements: Statements): GC[Token] =
    def renderRestOpt(restOpt: Option[Ps[Statements]]): GC[Token] =
      restOpt match {
        case None => pure(Chain.empty) | tok(Token.Punctuation(Punctuation.`;`))
        case Some(statements) =>
          renderStatements(statements.value)
          | (tok(Token.Punctuation(Punctuation.`;`)) ++ renderStatements(statements.value))
      }

    statements match {
      case Statements.Let(name, binding, restOpt) =>
        tok(Token.Keyword(Keyword.`let`))
        ++ tok(Token.Name(name.value))
        ++ renderBinding(binding.value, needEquals = true)
        ++ renderRestOpt(restOpt)
      case Statements.Var(name, binding, restOpt) =>
        tok(Token.Keyword(Keyword.`var`))
        ++ tok(Token.Name(name.value))
        ++ renderValBinding(binding.value, needEquals = true)
        ++ renderRestOpt(restOpt)
      case Statements.Single(singleStatement, restOpt) =>
        renderSingleStatement(singleStatement.value)
        ++ renderRestOpt(restOpt)
    }

  private def renderSingleStatement(statement: SingleStatement): GC[Token] =
    statement match {
      case SingleStatement.Await(expression) =>
        tok(Token.Keyword(Keyword.`await`))
        ++ renderExpression(expression.value)
      case SingleStatement.Assignment(pairs) =>
        renderSepBy(pairs)(Chain.one(Token.Punctuation(Punctuation.`||`))) {
          case Ps(AssignPair(lhs, binding)) =>
            renderAssignLhs(lhs.value)
            ++ (binding.value match {
              case needsEq @ (_: ValBinding.Value | _: ValBinding.Call) => 
                tok(Token.Punctuation(Punctuation.`:=`))
                ++ renderValBinding(needsEq, needEquals = false)
              case noEq =>
                renderValBinding(noEq, needEquals = false)
            })
        }
      case SingleStatement.Block(statements) =>
        statements match {
          case None => renderBraces(pure(Chain.empty))
          case Some(statements) =>
            renderBraces(renderStatements(statements.value))
        }
      case SingleStatement.If(predicate, thenBlock, elseBlockOpt) =>
        tok(Token.Keyword(Keyword.`if`))
        ++ tok(Token.Punctuation(Punctuation.`(`))
        ++ renderExpression(predicate.value)
        ++ tok(Token.Punctuation(Punctuation.`)`))
        ++ renderSingleStatement(thenBlock.value)
        ++ (elseBlockOpt match {
          case None => pure(Chain.empty)
          case Some(Ps(elseBlock)) =>
            tok(Token.Keyword(Keyword.`else`))
            ++ renderSingleStatement(elseBlock)
        })
      case SingleStatement.Call(path, arguments) =>
        tok(Token.Keyword(Keyword.`call`))
        ++ renderPath(path.value)
        ++ renderParentheses(renderCommaSep(arguments)(arg => renderCallArg(arg.value)))
      case SingleStatement.Assert(property) =>
        tok(Token.Keyword(Keyword.assert))
        ++ renderExpression(property.value)
      case SingleStatement.Assume(property) =>
        tok(Token.Keyword(Keyword.assume))
        ++ renderExpression(property.value)
      case SingleStatement.Defer(label) =>
        tok(Token.Keyword(Keyword.`defer`))
        ++ tok(Token.Name(label.value))
      case SingleStatement.Either(blocks) =>
        tok(Token.Keyword(Keyword.either))
        ++ renderSingleStatement(blocks.head.value)
        ++ blocks.tail.foldLeft(pure(Chain.empty)) { (acc, blk) =>
          acc
          ++ tok(Token.Keyword(Keyword.or))
          ++ renderSingleStatement(blk.value)
        }
      case SingleStatement.Fork(bindings, blocks) =>
        tok(Token.Keyword(Keyword.fork))
        ++ (if(bindings.nonEmpty) renderParentheses(renderCommaSep(bindings)(binding => renderQuantifierBound(binding.value))) else pure(Chain.empty))
        ++ renderSepBy(blocks)(Chain.one(Token.Keyword(Keyword.and)))(block => renderSingleStatement(block.value))
      case SingleStatement.LocalDefinition(defn) => 
        renderDefinition(defn.value)
      case SingleStatement.Return(value) =>
        tok(Token.Keyword(Keyword.`return`))
        ++ renderValBinding(value.value, needEquals = false)
      case SingleStatement.Spawn(bindings, block) =>
        tok(Token.Keyword(Keyword.spawn))
        ++ (if(bindings.nonEmpty) renderParentheses(renderCommaSep(bindings)(binding => renderQuantifierBound(binding.value))) else pure(Chain.empty))
        ++ renderSingleStatement(block.value)
    }
    
  private def renderCheckDirective(directive: CheckDirective): GC[Token] =
    directive match {
      case CheckDirective.CheckTimeStatements(statements) =>
        renderStatements(statements.value)
      case CheckDirective.OverrideLet(path, binding) =>
        tok(Token.Keyword(Keyword.`override`))
        ++ tok(Token.Keyword(Keyword.`let`))
        ++ renderPath(path.value)
        ++ renderBinding(binding.value, needEquals = true)
      case CheckDirective.OverrideVar(path, expr) =>
        tok(Token.Keyword(Keyword.`override`))
        ++ tok(Token.Keyword(Keyword.`let`))
        ++ renderPath(path.value)
        ++ tok(Token.Punctuation(Punctuation.`=`))
        ++ renderExpression(expr.value)
    }

  private def renderParam(param: Param): GC[Token] =
    param match {
      case Param.Name(name) =>
        tok(Token.Name(name.value))
      case Param.Impl(aliasOpt, interface) =>
        aliasOpt.fold(pure(Chain.empty)) { alias =>
          tok(Token.Name(alias.value))
        }
        ++ tok(Token.Keyword(Keyword.impl))
        ++ renderPath(interface.value)
    }

  private def renderQualifier(qualifier: Definition.Qualifier): GC[Token] =
    qualifier match {
      case Definition.Qualifier.Async => tok(Token.Keyword(Keyword.async))
      case Definition.Qualifier.Pure => tok(Token.Keyword(Keyword.pure))
    }

  private def renderInterfaceMember(member: InterfaceMember): GC[Token] =
    member match {
      case InterfaceMember.AbstractLet(name) =>
        tok(Token.Keyword(Keyword.`abstract`))
        ++ tok(Token.Keyword(Keyword.`let`))
        ++ tok(Token.Name(name.value))
      case InterfaceMember.AbstractVar(name) =>
        tok(Token.Keyword(Keyword.`abstract`))
        ++ tok(Token.Keyword(Keyword.`var`))
        ++ tok(Token.Name(name.value))
      case InterfaceMember.AbstractDef(qualifierOpt, name, params) =>
        tok(Token.Keyword(Keyword.`abstract`))
        ++ qualifierOpt.fold(pure(Chain.empty))(qual => renderQualifier(qual.value))
        ++ tok(Token.Keyword(Keyword.`def`))
        ++ tok(Token.Name(name.value))
        ++ renderParentheses(renderCommaSep(params)(param => renderParam(param.value)))
      case InterfaceMember.ConcreteStatements(statements) =>
        renderStatements(statements.value)
    }

  private def renderCheckBlock(checkBlock: CheckBlock): GC[Token] =
    renderBlockLike(checkBlock.directives)(dir => renderCheckDirective(dir.value))

  private def renderInterfaceRef(ref: InterfaceRef): GC[Token] =
    renderPath(ref.path.value)

  private def renderInterfaceBlock(interfaceBlock: InterfaceBlock): GC[Token] =
    renderBlockLike(interfaceBlock.members)(member => renderInterfaceMember(member.value))

  private def renderImplBlock(implBlock: ImplBlock): GC[Token] =
    renderBlockLike(implBlock.definitions)(definition => renderDefinition(definition.value))

  private def renderDefinition(definition: Definition): GC[Token] =
    definition match {
      case Definition.Def(qualifierOpt, name, params, body) =>
        qualifierOpt.fold(pure(Chain.empty))(qual => renderQualifier(qual.value))
        ++ tok(Token.Keyword(Keyword.`def`))
        ++ tok(Token.Name(name.value))
        ++ renderParentheses(renderCommaSep(params)(param => renderParam(param.value)))
        ++ renderSingleStatement(body.value)
      case Definition.Check(name, block) =>
        tok(Token.Keyword(Keyword.check))
        ++ tok(Token.Name(name.value))
        ++ renderCheckBlock(block.value)
      case Definition.Interface(name, supers, block) =>
        tok(Token.Keyword(Keyword.`interface`))
        ++ tok(Token.Name(name.value))
        ++ (if(supers.nonEmpty) {
          tok(Token.Keyword(Keyword.`extends`))
          ++ renderCommaSep(supers)(sup => renderInterfaceRef(sup.value))
        } else pure(Chain.empty))
        ++ renderInterfaceBlock(block.value)
      case Definition.Impl(name, params, interfaces, block) =>
        tok(Token.Keyword(Keyword.impl))
        ++ tok(Token.Name(name.value))
        ++ renderParentheses(renderCommaSep(params)(param => renderParam(param.value)))
        ++ (if(interfaces.nonEmpty) {
          tok(Token.Keyword(Keyword.`extends`))
          ++ renderCommaSep(interfaces)(sup => renderInterfaceRef(sup.value))
        } else pure(Chain.empty))
        ++ renderImplBlock(block.value)
    }

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

  private given errorOps: ErrorOps[Parser.Elem, Parser.Error] =
    Parser.errorOps

  abstract class ToTokensAndBack[T] {
    def gen: Generator[T]

    def render(t: T): GC[Token]

    def parse(input: Parser.Input): Either[NonEmptyList[Parser.Error], T]

    def applyConstraints(checker: Checker[(T, List[Token])]): checker.Self

    def run(): Unit =
      applyConstraints {
        gen
          .andThen { t =>
            // get all possible token renderings of the generated AST
            pure(t).product(render(t).force_!.map(_.toList))
          }
          .toChecker
          .withPrintExamples(printExamples = false)
      }
        .forall {
          case (expectedT, tokens) =>
            val result = parse(
              TokenInput[Parser.Elem, Parser.Error](
                EvalList.fromIterableOnce(
                  tokens
                    .iterator
                    .zipWithIndex
                    .map { case (tok, idx) => Ps(tok)(using SourceLocation.fileStart("<dummy>", idx)) }
                    .map(Right(_))),
                initialSourceLocation = SourceLocation.fileStart(path = "<dummy>"),
                elemLocation = _.sourceLocation,
              )
            )
                
            assertEquals(result, Right(expectedT), clue = result match {
              case Left(errors) =>
                errors
                  .iterator
                  .map {
                    case ParserError.FromTokenizer(errors) => s"tokenizer: $errors"
                    case ParserError.UnexpectedEOF(sourceLocation) => s"unexpected EOF at $sourceLocation"
                    case ParserError.ExpectedAbstract(category, actualTok) => s"expected $category at ${actualTok.sourceLocation}, got ${actualTok.value}"
                    case ParserError.ExpectedKeyword(expectedKeyword, actualTok) => s"expected $expectedKeyword at ${actualTok.sourceLocation}, got ${actualTok.value}"
                    case ParserError.ExpectedPunctuation(expectedPunctuation, actualTok) => s"expected $expectedPunctuation at ${actualTok.sourceLocation}, got ${actualTok.value}"
                  }
                  .mkString("\n")
              case Right(_) => "didn't match"
            })
        }
        .run()
  }

  // note: use single token strings / bigints because the parser is insensitive to the values

  abstract class ModuleAndBack extends ToTokensAndBack[Ps[Module]] {
    def parse(input: Parser.Input): Either[NonEmptyList[Parser.Error], Ps[Module]] =
      Parser.module.parse(input).map(_._1)

    def render(t: Ps[Module]): Generator[Chain[Token]] =
      renderModule(t.value)
  }

  test("to tokens and back (fully general)") {
    new ModuleAndBack {
      def gen: Generator[Ps[Module]] =
        Generatable[Ps[Module]]
          .build
          .replace[Int](pure(-1))
          .replace[BigInt](pure(BigInt(0)))
          .replace[String](pure("str"))
          .apply

      def applyConstraints(checker: Checker[(Ps[Module], List[Token])]): checker.Self =
        checker
          .exists {
            case (_, tokens) => tokens.size >= 50
          }
    }
    .run()
  }

  test("to tokens and back (focus on one definition)") {
    new ModuleAndBack {
      def gen: Generator[Ps[Module]] =
        Generatable[Ps[Module]]
          .build
          .replace[Int](pure(-1))
          .replace[BigInt](pure(BigInt(0)))
          .replace[String](pure("str"))
          .replace[List[Ps[Definition]]] {
            listOf(Generatable[Ps[Definition]].any, limit = 1)
          }
          .replace[List[Ps[Param]]] {
            listOf(Generatable[Ps[Param]].any, limit = 2)
          }
          .apply

      def applyConstraints(checker: Checker[(Ps[Module], List[Token])]): checker.Self =
        checker
          .exists {
            case (module, _) =>
              Transformable[Ps[Module]]
                .combining[Count]
                .incrementAt[Ps[SingleStatement]](_ => true)
                .apply(module)
                .depth >= 2
          }
    }
    .run()
  }

  test("to tokens and back (expression)") {
    new ToTokensAndBack[Ps[Expression]] {
      def gen: Generator[Ps[Expression]] =
        Generatable[Ps[Expression]]
          .build
          .replace[Int](pure(-1))
          .replace[BigInt](pure(BigInt(0)))
          .replace[String](pure("str"))
          .apply

      def applyConstraints(checker: Checker[(Ps[Expression], List[Token])]): checker.Self =
        checker
          .exists {
            case (_, tokens) => !tokens.exists {
              // if I messed up and all the exprs have () around them, this will fail
              case Token.Punctuation(Punctuation.`(` | Punctuation.`)`) => true
              case _ => false
            }
          }
          .exists {
            case (expr, _) =>
              Transformable[Ps[Expression]]
                .combining[Count]
                .incrementAt[Ps[Expression]](_ => true)
                .apply(expr)
                .depth >= 2
          }

      def parse(input: Parser.Input): Either[NonEmptyList[Parser.Error], Ps[Expression]] =
        Parser.expression.parse(input).map(_._1)

      def render(t: Ps[Expression]): Generator[Chain[Token]] =
        renderExpression(t.value)
    }
    .run()
  }

  test("to tokens and back (statement)".only) {
    new ToTokensAndBack[Ps[Statements]] {
      def gen: Generator[Ps[Statements]] =
        Generatable[Ps[Statements]]
          .build
          .replace[Int](pure(-1))
          .replace[BigInt](pure(BigInt(0)))
          .replace[String](pure("str"))
          .apply

      def applyConstraints(checker: Checker[(Ps[Statements], List[Token])]): checker.Self =
        checker
          .exists {
            case (stmt, _) =>
              Transformable[Ps[Statements]]
                .combining[Count]
                .incrementAt[Ps[SingleStatement]](_ => true)
                .apply(stmt)
                .depth >= 3
          }

      def parse(input: Parser.Input): Either[NonEmptyList[Parser.Error], Ps[Statements]] =
        Parser.statements.parse(input).map(_._1)

      def render(t: Ps[Statements]): Generator[Chain[Token]] =
        renderStatements(t.value)
    }
    .run()
  }
}
