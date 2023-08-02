package test.com.github.distcompiler.dcal

import cats.data.Chain
import java.time.Duration

import utest.*
import chungus.{Generator, Checker}
import com.github.distcompiler.dcal.{DCalAST, DCalTokenizer, DCalParser}
import com.github.distcompiler.dcal.parsing.{Ps, SourceLocation}

object DCalParserTests extends TestSuite {
  import DCalAST.*
  import DCalTokenizer.*
  import Generator.*

  private type GC[T] = Generator[Chain[T]]

  private given dummyLoc: SourceLocation = SourceLocation("dummy", offsetStart = -1, offsetEnd = -1)

  // use single token strings / bigints because the parser is insensitive to the values
  private given strGen: Generator[String] = one("foo")
  private given numGen: Generator[BigInt] = one(BigInt(1))

  private given psGen[T](using gen: Generator[T]): Generator[Ps[T]] = gen.map(Ps(_))

  // if we focus on generating lists that are too long we'll never get very deep. length <= 3 seems good here.
  private given limitedListOf[T](using gen: Generator[T]): Generator[List[T]] = listOf(gen, limit = 3)

  extension [FA](self: FA)(using isIterableOnce: collection.generic.IsIterableOnce[FA]) {
    private def sepBy(sep: isIterableOnce.A): Iterator[isIterableOnce.A] =
      self.sepByAll(Some(sep): Iterable[isIterableOnce.A])

    private def sepByAll(sep: Iterable[isIterableOnce.A]): Iterator[isIterableOnce.A] =
      isIterableOnce(self).iterator
        .scanLeft(None: Option[Iterator[isIterableOnce.A]]) { (prevOpt, elem) =>
        prevOpt match {
          case None =>
            Some(Iterator.single(elem))
          case Some(_) =>
            Some(sep.iterator ++ Iterator.single(elem))
        }
      }
      .flatten
      .flatten
  }

  private def renderImports(imports: List[Ps[String]]): GC[Token] =
    imports match {
      case Nil => one(Chain.nil)
      case imports =>
        chainCat(
          Token.Keyword(Keyword.`import`),
          imports.iterator
            .map(_.value)
            .map(Token.Name(_))
            .sepBy(Token.Punctuation(Punctuation.`,`)),
        )
    }

  private def renderPath(path: Path): GC[Token] =
    path match {
      case Path.Name(name) =>
        chainCat(Token.Name(name))
      case Path.Project(prefix, name) =>
        chainCat(
          renderPath(prefix.value),
          Token.Punctuation(Punctuation.`.`),
          Token.Name(name),
        )
      case Path.Index(prefix, index) =>
        chainCat(
          renderPath(prefix.value),
          Token.Punctuation(Punctuation.`[`),
          renderExpression(index.value),
          Token.Punctuation(Punctuation.`]`),
        )
    }

  private def renderExpression(expression: Expression, needGroup: Boolean = false): GC[Token] =
    expression match {
      case Expression.IntLiteral(value) =>
        one(Chain.one(Token.IntLiteral(value)))
      case Expression.StringLiteral(value) =>
        one(Chain.one(Token.StringLiteral(value)))
      case Expression.OpCall(ref, arguments) =>
        ref match {
          case Left(op) =>
            val List(lhs, rhs) = arguments
            val mainPart =
              chainCat[Token](
                renderExpression(lhs.value, needGroup = !needGroup),
                Token.BinaryOperator(op.value),
                renderExpression(rhs.value, needGroup = !needGroup),
              )

            if(needGroup) {
              chainCat(
                Token.Punctuation(Punctuation.`(`),
                mainPart,
                Token.Punctuation(Punctuation.`)`),
              )
            } else {
              mainPart
            }
          case Right(path) =>
            chainCat(
              renderPath(path.value),
              if(arguments.nonEmpty) {
                chainCat[Token](
                  Token.Punctuation(Punctuation.`(`),
                  arguments.iterator
                    .map(expr => renderExpression(expr.value))
                    .sepBy(one(Chain.one(Token.Punctuation(Punctuation.`,`)))),
                  Token.Punctuation(Punctuation.`)`),
                )
              } else {
                None: Option[Token]
              },
            )
        }
      case Expression.SetConstructor(members) =>
        chainCat(
          Token.Punctuation(Punctuation.`{`),
          members.iterator
            .map(_.value)
            .map(renderExpression(_))
            .sepBy(one(Chain.one(Token.Punctuation(Punctuation.`,`)))),
          Token.Punctuation(Punctuation.`}`),
        )
    }

  private def renderBinding(binding: Binding, needEquals: Boolean = true): GC[Token] =
    binding match {
      case Binding.Value(expr) => 
        chainCat(
          if(needEquals) {
            Some(Token.BinaryOperator(BinaryOperator.`=`))
          } else {
            None: Option[Token]
          },
          renderExpression(expr.value),
        )
      case Binding.Selection(binding) =>
        chainCat(
          Token.BinaryOperator(BinaryOperator.`\\in`),
          renderBinding(binding.value, needEquals = false),
        )
      case Binding.Call(path, arguments) =>
        chainCat(
          if(needEquals) {
            Some(Token.BinaryOperator(BinaryOperator.`=`))
          } else {
            None: Option[Token]
          },
          Token.Keyword(Keyword.`call`),
          renderPath(path.value),
          Token.Punctuation(Punctuation.`(`),
          arguments.iterator
            .map(_.value)
            .map(renderExpression(_))
            .sepBy(one(Chain.one(Token.Punctuation(Punctuation.`,`)))),
          Token.Punctuation(Punctuation.`)`),
        )
    }

  private def renderStatement(statement: Statement): GC[Token] =
    statement match {
      case Statement.Await(expression) =>
        chainCat(
          Token.Keyword(Keyword.`await`),
          renderExpression(expression.value),
        )
      case Statement.Assignment(pairs) =>
        pairs.iterator
          .map(_.value)
          .map {
            case AssignPair(path, rhs) =>
              chainCat[Token](
                renderPath(path.value),
                Token.Punctuation(Punctuation.`:=`),
                renderExpression(rhs.value),
              )
          }
          .sepBy(one(Chain.one(Token.Punctuation(Punctuation.`||`))))
          .reduceOption(_ ++ _)
          .getOrElse(one(Chain.nil))
      case Statement.Let(name, binding) =>
        chainCat(
          Token.Keyword(Keyword.`let`),
          Token.Name(name.value),
          renderBinding(binding.value),
        )
      case Statement.Var(name, binding) =>
        chainCat(
          Token.Keyword(Keyword.`var`),
          Token.Name(name.value),
          renderBinding(binding.value),
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
          renderExpression(predicate.value),
          Token.Punctuation(Punctuation.`)`),
          renderStatement(thenBlock.value),
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
              .sepBy(Token.Punctuation(Punctuation.`,`)),
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

  private def astTokPairs: Generator[(Module, List[Token])] = {
    for {
      module <- anyOf[Module]
      // don't pay for rendering; limited combos come from module
      toks <- renderModule(module).force
    } yield (module, toks.toList)
  }
  
  def tests = Tests {
    test("to tokens and back") {
      astTokPairs.checkWith {
        import Checker.*
        timeLimited(maxDuration = Duration.ofMinutes(1), printRoundExample = false) {
          forall {
            case (expectedModule, tokens) =>
              val result = DCalParser(tokens.iterator.map(Ps(_)).map(Right(_)), path = "<dummy>")
              assert(result == Right(Ps(expectedModule)))
          }
        }
      }
    }
  }
}
