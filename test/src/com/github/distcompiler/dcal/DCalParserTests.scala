package test.com.github.distcompiler.dcal

import cats.data.Chain
import cats.syntax.all.given
import java.time.Duration

import utest.{TestSuite, Tests, test}
import chungus.*
import com.github.distcompiler.dcal.{DCalAST, DCalTokenizer, DCalParser, Token, Punctuation, Keyword, BinaryOperator}
import com.github.distcompiler.dcal.parsing.{Ps, SourceLocation}

object DCalParserTests extends TestSuite {
  import DCalAST.*
  import Generator.*

  private type GC[T] = Generator[Chain[T]]

  private def tok(tok: Token): GC[Token] =
    pure(Chain.one(tok))

  private def toks(toks: Token*): GC[Token] =
    pure(Chain.fromSeq(toks))

  private given dummyLoc: SourceLocation = SourceLocation("dummy", offsetStart = -1, offsetEnd = -1)

  // use single token strings / bigints because the parser is insensitive to the values
  private given strGen: Generator[String] = pure("foo")
  private given numGen: Generator[BigInt] = pure(BigInt(1))

  private given psGen[T](using gen: Generator[T]): Generator[Ps[T]] = gen.map(Ps(_))

  // object StmtKey extends Counters.Key[Statement]
  // private given stmtGen: Generator[Statement] =
  //   rateLimit(StmtKey, limit = 3)(Generator.anySum)

  // object ExprKey extends Counters.Key[Expression]
  // private given exprGen: Generator[Expression] =
  //   rateLimit(ExprKey, limit = 3)(Generator.anySum)

  // object BindingKey extends Counters.Key[Binding]
  // private given genBinding: Generator[Binding] =
  //   rateLimit(BindingKey, limit = 3)(Generator.anySum)

  // if we focus on generating lists that are too long we'll never get very deep. length <= 3 seems good here.
  private given limitedListOf[T](using gen: Generator[T]): Generator[List[T]] = listOf(gen, limit = 3)

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

  def toTokensAndBack(): Unit = {
    // TODO: the existence condition isn't strong enough; we should be sure to see e.g. some expressions! 
    anyOf[Module]
      .toChecker
      .transform { module =>
        // get tokens
        (module, renderModule(module).examplesIterator.map(_.value).toList)
      } { checker =>
        checker
          .exists {
            case (expectedModule, _) => expectedModule.definitions.size >= 2
          }
          .forall {
            case (expectedModule, tokenss) =>
              tokenss.foreach { tokens =>
                recording(tokens) {
                  val result = DCalParser(tokens.iterator.map(Ps(_)).map(Right(_)), path = "<dummy>")
                  recording(result) {
                    assert(result == Right(Ps(expectedModule)))
                  }
                }
              }
          }
      }
      .run()
  }
  
  def tests = Tests {
    test("to tokens and back") - toTokensAndBack()
  }
}
