package test.com.github.distcompiler.dcal

import cats.data.Chain

import utest.*
import chungus.Generator
import com.github.distcompiler.dcal.DCalAST
import com.github.distcompiler.dcal.parsing.{Ps, SourceLocation}
import com.github.distcompiler.dcal.DCalTokenizer
import scala.collection.IterableOnceOps
import java.awt.RenderingHints.Key
import com.github.distcompiler.dcal.DCalParser

object DCalParserTests extends TestSuite {
  import DCalAST.*
  import DCalTokenizer.*
  import Generator.*

  private type GC[T] = Generator[Chain[T]]

  private given dummyLoc: SourceLocation = SourceLocation("dummy", offsetStart = -1, offsetEnd = -1)

  private given strGen: Generator[String] = chooseAny(List("foo", "bar", "ping", "pong", "blamo"))
  private given numGen: Generator[BigInt] = chooseAny(List(BigInt(0), BigInt(10), BigInt(123456789), BigInt(10)))

  private given psGen[T](using gen: Generator[T]): Generator[Ps[T]] = gen.map(Ps(_))

  extension [T](self: Iterator[T]) private def sepBy(sep: Iterable[T]): Iterator[T] =
    self
      .scanLeft(None: Option[Iterator[T]]) { (prevOpt, elem) =>
        prevOpt match {
          case None =>
            Some(Iterator.single(elem))
          case Some(_) =>
            Some(sep.iterator ++ Iterator.single(elem))
        }
      }
      .flatten
      .flatten

  private def renderImports(imports: List[Ps[String]]): GC[Token] =
    imports match {
      case Nil => one(Chain.nil)
      case imports =>
        chainCat(
          Token.Keyword(Keyword.`import`),
          imports.iterator
            .map(_.value)
            .map(Token.Name(_))
            .sepBy(List(Token.Punctuation(Punctuation.`,`))),
        )
    }

  private def renderPath(path: Path): GC[Token] =
    path match {
      case Path.Name(name) =>
        chainCat(Token.Name(name))
      case Path.Project(prefix, name) =>
        chainCat(
          renderPath(prefix),
          Token.Punctuation(Punctuation.`.`),
          Token.Name(name),
        )
      case Path.Index(prefix, index) =>
        chainCat(
          renderPath(prefix),
          Token.Punctuation(Punctuation.`[`),
          renderExpression(index),
          Token.Punctuation(Punctuation.`]`),
        )
    }

  private def renderExpression(expression: Expression, needGroup: Boolean = false): GC[Token] =
    expression match {
      case Expression.PathRef(path) =>
        renderPath(path.value)
      case Expression.IntLiteral(value) =>
        one(Chain.one(Token.IntLiteral(value)))
      case Expression.StringLiteral(value) =>
        one(Chain.one(Token.StringLiteral(value)))
      case Expression.OpCall(path, arguments) =>
        one(Chain.nil) // stub
      case Expression.SetConstructor(members) =>
        chainCat(
          Token.Punctuation(Punctuation.`{`),
          members.iterator
            .map(_.value)
            .map(renderExpression(_))
            .sepBy(List(one(Chain.one(Token.Punctuation(Punctuation.`,`))))),
          Token.Punctuation(Punctuation.`}`),
        )
    }

  private def renderBinding(binding: Binding, needEquals: Boolean = true): GC[Token] =
    binding match {
      case Binding.Value(expr) => 
        chainCat(
          if(needEquals) {
            Some(Token.Operator(Operator.`=`))
          } else {
            None: Option[Token]
          },
          renderExpression(expr),
        )
      case Binding.Selection(binding) =>
        chainCat(
          Token.Operator(Operator.`\\in`),
          renderBinding(binding, needEquals = false),
        )
      case Binding.Call(path, arguments) =>
        chainCat(
          if(needEquals) {
            Some(Token.Operator(Operator.`=`))
          } else {
            None: Option[Token]
          },
          Token.Keyword(Keyword.`call`),
          renderPath(path),
          Token.Punctuation(Punctuation.`(`),
          arguments.iterator
            .map(_.value)
            .map(renderExpression(_))
            .sepBy(List(one(Chain.one(Token.Punctuation(Punctuation.`,`))))),
          Token.Punctuation(Punctuation.`)`),
        )
    }

  private def renderStatement(statement: Statement): GC[Token] =
    statement match {
      case Statement.Await(expression) =>
        chainCat(
          Token.Keyword(Keyword.`await`),
          renderExpression(expression),
        )
      case Statement.Assignment(pairs) =>
        pairs.iterator
          .map(_.value)
          .map {
            case AssignPair(path, rhs) =>
              ??? : GC[Token]
          }
          .reduceOption(_ ++ _)
          .getOrElse(one(Chain.nil))
      case Statement.Let(name, binding) =>
        chainCat(
          Token.Keyword(Keyword.`let`),
          Token.Name(name.value),
          renderBinding(binding),
        )
      case Statement.Var(name, binding) =>
        chainCat(
          Token.Keyword(Keyword.`var`),
          Token.Name(name.value),
          renderBinding(binding),
        )
      case Statement.Block(statements) =>
        chainCat(
          Token.Punctuation(Punctuation.`{`),
          statements.iterator
            .map(_.value)
            .map(renderStatement)
            .reduceOption(_ ++ _)
            .getOrElse(one(Chain.nil)),
          Token.Punctuation(Punctuation.`}`),
        )
      case Statement.If(predicate, thenBlock, elseBlockOpt) =>
        chainCat(
          Token.Keyword(Keyword.`if`),
          Token.Punctuation(Punctuation.`(`),
          renderExpression(predicate),
          Token.Punctuation(Punctuation.`)`),
          renderStatement(thenBlock),
          elseBlockOpt
            .map(_.value)
            .map {
              case block =>
                chainCat[Token](
                  Token.Keyword(Keyword.`else`),
                  renderStatement(block),
                )
            }
        )
      case Statement.Call(Ps(binding @ Binding.Call(_, _))) =>
        renderBinding(binding, needEquals = false)
    }

  private def renderDefinitions(definitions: List[Ps[Definition]]): GC[Token] =
    definitions.iterator
      .map(_.value)
      .map {
        case Definition(name, params, body) =>
          chainCat[Token](
            Token.Keyword(Keyword.`def`),
            Token.Name(name.value),
            Token.Punctuation(Punctuation.`(`),
            params.iterator
              .map(_.value)
              .map(Token.Name(_))
              .sepBy(List(Token.Punctuation(Punctuation.`,`))),
            Token.Punctuation(Punctuation.`)`),
            renderStatement(body.value),
          )
      }
      .reduceOption(_ ++ _)
      .getOrElse(one(Chain.nil))

  private def renderModule(module: Module): GC[Token] = {
    val Module(name, imports, definitions) = module

    chainCat(
      Token.Keyword(Keyword.`module`),
      Token.Name(name.value),
      renderImports(imports),
      renderDefinitions(definitions),
    )
  }

  private def astTokPairs: Generator[(Module, Chain[Token])] = {
    for {
      module <- anyOf[Module]
      // don't pay for rendering; limited combos come from module
      toks <- renderModule(module).force
    } yield (module, toks)
  }
  
  def tests = Tests {
    test("to tokens and back") {
      astTokPairs.forall(budget = 20) {
        case (expectedModule, tokens) =>
          val result = DCalParser(tokens.iterator.map(Ps(_)).map(Right(_)), path = "<dummy>")
          assert(result == Right(Ps(expectedModule)))
      }
    }
  }
}
