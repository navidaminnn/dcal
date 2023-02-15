package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.AST.*
import com.github.distcompiler.dcal.DCalTokenizer.*

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object DCalParser {
  object parse extends Parsers {
    override type Elem = TokenData

    class TokensReader(elems: LazyList[Token]) extends Reader[TokenData] {
      override def first: TokenData = elems.head.data

      override def atEnd: Boolean = elems.isEmpty

      override def pos: Position = NoPosition

      override def rest: Reader[TokenData] = new TokensReader(elems.tail)
    }

    def apply(tokens: Iterator[Token]): AST.Module = {
      val reader = new TokensReader(tokens.to(LazyList))
      module(reader) match {
        case Success(mdl, _) => mdl
        case Failure(msg, _) => ???
        case Error(msg, _) => ???
      }
    }

    lazy val name: Parser[String] = {
      acceptMatch("name", { case TokenData.Name(name) => name })
    }

    /**
     *
     * @param elemParser      a parser that matches an element of type T
     * @param delimiterParser a parser that matches an element of type D
     * @tparam T              the type of elements if successfully parsed
     * @tparam D              the type of delimiter elements
     * @return the parser that matches a delimited sequence of elements of type T
     */
    private def delimited[T, D](elemParser: =>Parser[T], delimiterParser: Parser[D] = TokenData.Comma): Parser[List[T]] = {
      (elemParser ~ rep(delimiterParser ~> elemParser)).map{
        case t ~ ts => t :: ts
      }
    }

    lazy val module: Parser[AST.Module] = {
      (elem(TokenData.Module) ~> name ~ opt(imports) ~ rep(definition)).map {
        case name ~ importsOpt ~ definitions =>
          AST.Module(
            name = name,
            imports = importsOpt.getOrElse(Nil),
            definitions = definitions,
          )
      }
    }

    lazy val imports: Parser[List[String]] = {
      (elem(TokenData.Import) ~> delimited(name)).map {
        case imports => imports
      }
    }

    lazy val definition: Parser[AST.Definition] = {
      (elem(TokenData.Def) ~> name ~ elem(TokenData.OpenParenthesis) ~ opt(delimited(name)) ~ elem(TokenData.CloseParenthesis) ~ block).map {
        case name ~ _ ~ params ~ _ ~ block =>
          AST.Definition(
            name = name, params = params.getOrElse(Nil), body = block
          )
      }
    }

    lazy val block: Parser[AST.Block] = {
      (elem(TokenData.OpenCurlyBracket) ~> rep(statement) <~ elem(TokenData.CloseCurlyBracket))
        .map { statements =>
          AST.Block(statements)
        }
    }

    lazy val assignPair: Parser[AST.AssignPair] = (name ~ elem(TokenData.Walrus) ~ expression).map {
      case name ~ _ ~ expr => AST.AssignPair(name = name, expression = expr)
    }

    lazy val statement: Parser[AST.Statement] = {
      val await = (elem(TokenData.Await) ~> expression).map(expr => AST.Statement.Await(expr))

      val assignPairs =
        delimited(elemParser = assignPair, delimiterParser = elem(TokenData.DoublePipe)).map{
          pairs => AST.Statement.AssignPairs(pairs)
        }

      val let = (elem(TokenData.Let) ~> name ~ elem(TokenData.Equals) ~ expression).map {
        case name ~ _ ~ expr => AST.Statement.Let(name = name, expression = expr)
      }

      val op = acceptMatch("var operator", {
        case TokenData.Equals => AST.BinOp.Equals
        case TokenData.SlashIn => AST.BinOp.SlashIn
      })

      val `var` =
        (elem(TokenData.Var) ~> name ~ opt(op ~ expression)).map {
          case name ~ Some(opOpt ~ exprOpt) => AST.Statement.Var(name = name, opExpression = Some((opOpt, exprOpt)))
          case name ~ None => AST.Statement.Var(name = name, opExpression = None)
        }

      await | let | `var` | assignPairs
    }

    lazy val expression: Parser[AST.Expression] = expressionBinOp

    lazy val expressionBinOp: Parser[AST.Expression] = {
      (expressionBase ~ opt(binOp ~ expressionBase)).map {
        case (str @ AST.Expression.StringLiteral(_)) ~ None => str
        case (int @ AST.Expression.IntLiteral(_)) ~ None => int
        case (name @ AST.Expression.Name(_)) ~ None => name
        case left ~ Some(binOp ~ right) => AST.Expression.ExpressionBinOp(
          left = left,
          binOp = binOp,
          right = right
        )
      }
    }

    lazy val expressionBase: Parser[AST.Expression] = {
      val intLiteral = acceptMatch(
        "intLiteral",
        { case TokenData.IntLiteral(v) => AST.Expression.IntLiteral(v) }
      )

      val stringLiteral = acceptMatch(
        "stringLiteral",
        { case TokenData.StringLiteral(v) => AST.Expression.StringLiteral(v) }
      )

      val nameExpr = name.map(str => AST.Expression.Name(str))

      val bracketedExpr = (elem(TokenData.OpenParenthesis) ~> expression <~ elem(TokenData.CloseParenthesis)).map{ expr => expr }

      bracketedExpr | intLiteral | stringLiteral | nameExpr
    }

    lazy val binOp: Parser[AST.BinOp] =
      acceptMatch("binOp", { case TokenData.BinOpPlaceholder => AST.BinOp.Placeholder } )

  }

  def apply(contents: String, fileName: String): AST.Module =
    val tokens = tokenize(chars = contents, fileName = fileName)
    parse(tokens)
}
