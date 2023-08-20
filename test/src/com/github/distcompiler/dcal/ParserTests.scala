package test.com.github.distcompiler.dcal

import cats.*
import cats.data.Chain
import cats.syntax.all.given

import chungus.*
import com.github.distcompiler.dcal.util.EvalList
import com.github.distcompiler.dcal.{AST, Tokenizer, Parser, Token, Punctuation, Keyword, BinaryOperator}
import com.github.distcompiler.dcal.parsing.{Ps, SourceLocation}
import com.github.distcompiler.dcal.transform.Transform
import com.github.distcompiler.dcal.transform.instances.all.given

class ParserTests extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(1, scala.concurrent.duration.HOURS)

  import AST.*
  import Generator.*
  import Checker.*

  private given dummyLoc: SourceLocation = SourceLocation("dummy", offsetStart = -1, offsetEnd = -1)

  // use single token strings / bigints because the parser is insensitive to the values
  private given strGen: Generator[String] = pure("foo")
  private given numGen: Generator[BigInt] = pure(BigInt(1))

  private given psGen[T](using gen: Generator[T]): Generator[Ps[T]] = gen.map(Ps(_))

  // if we focus on generating lists that are too long we'll never get very deep. length <= 3 seems good here.
  private given limitedListOf[T](using gen: Generator[T]): Generator[List[T]] = listOf(gen, limit = 3)

  private type GC[T] = Generator[Chain[T]]

  private def tok(tok: Token): GC[Token] =
    pure(Chain.one(tok))

  private def toks(toks: Token*): GC[Token] =
    pure(Chain.fromSeq(toks))

  private def renderSepBy[T,U](seq: Seq[T])(sep: Chain[U])(fn: T => Generator[Chain[U]]): GC[U] =
    Chain.fromSeq(seq)
      .traverse[Generator, Chain[Chain[U]]](elem => fn(elem).map(Chain.one))
      .map(_.intercalate(Chain.one(sep)).flatten)

  private def renderCommaSep[T](seq: Seq[T])(fn: T => GC[Token]): GC[Token] =
    renderSepBy(seq)(Chain.one(Token.Punctuation(Punctuation.`,`)))(fn)

  private def renderImports(imports: List[Ps[Import]]): GC[Token] =
    imports match {
      case Nil => pure(Chain.nil)
      case imports =>
        tok(Token.Keyword(Keyword.`import`))
        ++ renderCommaSep(imports) {
          case Ps(Import.Name(name)) => pure(Chain.one(Token.Name(name)))
        }
    }

  private def renderPath(path: Path): GC[Token] =
    path match {
      case Path.Name(name) =>
        tok(Token.Name(name))
      case Path.Project(prefix, name) =>
        renderPath(prefix.value)
        ++ toks(
          Token.Punctuation(Punctuation.`.`),
          Token.Name(name),
        )
      case Path.Index(prefix, index) =>
        renderPath(prefix.value)
        ++ tok(Token.Punctuation(Punctuation.`[`))
        ++ renderExpression(index.value)
        ++ tok(Token.Punctuation(Punctuation.`]`))
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
        case Expression.OpCall(ref, arguments) =>
          ref match {
            case Left(op) =>
              val List(lhs, rhs) = arguments
              
              renderExpression(lhs.value, needGroup = true)
              ++ tok(Token.BinaryOperator(op.value))
              ++ renderExpression(rhs.value, needGroup = true)
            case Right(path) =>
              renderPath(path.value)
              ++ (if(arguments.nonEmpty) {
                tok(Token.Punctuation(Punctuation.`(`))
                ++ renderCommaSep(arguments)(expr => renderExpression(expr.value))
                ++ tok(Token.Punctuation(Punctuation.`)`))
              } else pure(Chain.empty))
          }
        case Expression.SetConstructor(members) =>
          tok(Token.Punctuation(Punctuation.`{`))
          ++ renderCommaSep(members)(expr => renderExpression(expr.value))
          ++ tok(Token.Punctuation(Punctuation.`}`))
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
      case Binding.Call(path, arguments) =>
        (if(needEquals) {
          tok(Token.BinaryOperator(BinaryOperator.`=`))
        } else {
          pure(Chain.empty)
        })
        ++ tok(Token.Keyword(Keyword.`call`))
        ++ renderPath(path.value)
        ++ tok(Token.Punctuation(Punctuation.`(`))
        ++ renderCommaSep(arguments)(arg => renderExpression(arg.value))
        ++ tok(Token.Punctuation(Punctuation.`)`))
    }

  private def renderStatement(statement: Statement): GC[Token] =
    statement match {
      case Statement.Await(expression) =>
        tok(Token.Keyword(Keyword.`await`))
        ++ renderExpression(expression.value)
      case Statement.Assignment(pairs) =>
        renderSepBy(pairs)(Chain.one(Token.Punctuation(Punctuation.`||`))) {
          case Ps(AssignPair(path, rhs)) =>
            renderPath(path.value)
            ++ tok(Token.Punctuation(Punctuation.`:=`))
            ++ renderExpression(rhs.value)
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
      case Statement.Call(Ps(binding @ Binding.Call(_, _))) =>
        renderBinding(binding, needEquals = false)
    }

  private def renderDefParam(param: Ps[DefParam]): GC[Token] =
    param.value match {
      case DefParam.Name(name) =>
        tok(Token.Name(name))
    }

  private def renderDefinitions(definitions: List[Ps[Definition]]): GC[Token] =
    Chain.fromIterableOnce(definitions)
      .flatTraverse[Generator, Token] {
        case Ps(Definition(name, params, body)) =>
          tok(Token.Keyword(Keyword.`def`))
          ++ tok(Token.Name(name.value))
          ++ tok(Token.Punctuation(Punctuation.`(`))
          ++ renderCommaSep(params)(renderDefParam)
          ++ tok(Token.Punctuation(Punctuation.`)`))
          ++ renderStatement(body.value)
      }

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
  

  test("to tokens and back (fully general)") {
    toTokensAndBack(anyOf[Module]) { checker =>
      checker
        .exists {
          case (_, tokens) => tokens.size >= 50
        }
    }
  }
  
  test("to tokens and back (focus on one definition)") {
    given Transform[String, Involves] = Transform.fromFunction(_ => Monoid[Involves].empty)
    given Transform[BigInt, Involves] = Transform.fromFunction(_ => Monoid[Involves].empty)

    toTokensAndBack {
      given anyDefns: Generator[List[Ps[Definition]]] =
        listOf(anyOf[Ps[Definition]], limit = 1)

      given anyDefParams: Generator[List[Ps[DefParam]]] =
        listOf(anyOf[Ps[DefParam]], limit = 2)

      anyOf[Module]
    } { checker =>
      checker
        .exists {
          case (module, _) =>
            // make sure we generate at least one level of nesting for exprs (same for stmts below)
            module
              .involves[Ps[Expression]](_ => true)
              .depth >= 2
        }
        .exists {
          case (module, _) =>
            module
              .involves[Ps[Statement]](_ => true)
              .depth >= 2
        }
    }
  }
}
