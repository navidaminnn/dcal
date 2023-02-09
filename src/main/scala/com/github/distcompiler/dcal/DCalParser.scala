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

    val name: Parser[String] = {
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
    def delimited[T, D](elemParser: Parser[T], delimiterParser: Parser[D] = TokenData.Comma): Parser[List[T]] = {
      (elemParser ~ rep(delimiterParser ~> elemParser)).map{
        case t ~ ts => t :: ts
      }
    }

    val module: Parser[AST.Module] = {
      // Caution: module is a function of type Parser[AST.Module]
      // so the expression assigned to module must produce a Parser[AST.Module]
      // The module parser is produced by sequencing smaller parsers, NOT tokens
      // Also, the operators ~ and ~> do not exist on Token, so Token.Module ~ TokenData.Name
      // is invalid while elem(TokenData.Module) ~ elem(TokenData.Name) is, because elem(...)
      // returns a Parser[this.elem]
      (elem(TokenData.Module) ~> name ~ opt(imports) ~ rep(definition)).map {
        case name ~ importsOpt ~ definitions =>
          AST.Module(
            name = name,
            imports = importsOpt.getOrElse(Nil),
            definitions = definitions,
          )
      }
    }

    val imports: Parser[List[String]] = {
      (elem(TokenData.Import) ~> delimited(name)).map {
        case imports => imports
      }
    }

    val definition: Parser[AST.Definition] = {
      (elem(TokenData.Def) ~> name ~ elem(TokenData.OpenParenthesis) ~ delimited(name) ~ elem(TokenData.CloseParenthesis) ~ block).map {
        case name ~ _ ~ args ~ _ ~ block =>
          AST.Definition(
            name = name, args = args, block = block
          )
      }
    }

    def block: Parser[AST.Block] = {
      (elem(TokenData.OpenCurlyBracket) ~> rep(statement) <~ elem(TokenData.CloseCurlyBracket)).map {
        case statements => AST.Block(statements)
      }
    }

    def statement: Parser[AST.Statement] = {
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
        case TokenData.Walrus => AST.BinOp.SlashIn
      })
      val `var` =
        (elem(TokenData.Var) ~> name ~ opt(op ~ expression)).map {
          case name ~ Some(opOpt ~ exprOpt) => AST.Statement.Var(name = name, opExpression = Some((opOpt, exprOpt)))
          case name ~ None => AST.Statement.Var(name = name, opExpression = None)
        }

      await | let | `var` | assignPairs
    }

    def assignPair: Parser[AST.AssignPair] = (name ~ elem(TokenData.Walrus) ~ expression).map {
        case name ~ _ ~ expr => AST.AssignPair(name = name, expression = expr)
    }

    def expression: Parser[AST.Expression] = {
      val expressionBinOp = (expression ~ binOp ~ expression).map {
        case left ~ binOp ~ right => AST.Expression.ExpressionBinOp(left = left, binOp = binOp, right = right)
      }

      val intLiteral = acceptMatch(
        "intLiteral",
        { case TokenData.IntLiteral(v) => AST.Expression.IntLiteral(v) }
      )

      val stringLiteral = acceptMatch(
        "stringLiteral",
        { case TokenData.StringLiteral(v) => AST.Expression.StringLiteral(v) }
      )

      val nameExpr = name.map(str => AST.Expression.Name(str))

      expressionBinOp | intLiteral | stringLiteral | nameExpr
    }

    def binOp: Parser[AST.BinOp] =
      acceptMatch("binOp", { case TokenData.BinOpPlaceholder => AST.BinOp.TODO } )
  }

  def apply(contents: String, fileName: String): AST.Module =
    val tokens = tokenize(chars = contents, fileName = fileName)
    parse(tokens)
}
