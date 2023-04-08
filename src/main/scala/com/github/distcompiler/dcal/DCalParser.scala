package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.*
import com.github.distcompiler.dcal.DCalTokenizer.*

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object DCalParser {
  object parse extends Parsers {
    override type Elem = DCalTokenData

    class TokensReader(elems: LazyList[Token]) extends Reader[DCalTokenData] {
      override def first: DCalTokenData = elems.head.data

      override def atEnd: Boolean = elems.isEmpty

      override def pos: Position = NoPosition

      override def rest: Reader[DCalTokenData] = new TokensReader(elems.tail)
    }

    def apply(tokens: Iterator[Token]): DCalAST.Module = {
      val reader = new TokensReader(tokens.to(LazyList))
      module(reader) match {
        case Success(mdl, _) => mdl
        case Failure(msg, _) => ???
        case Error(msg, _) => ???
      }
    }

    lazy val name: Parser[String] = {
      acceptMatch("name", { case DCalTokenData.Name(name) => name })
    }

    /**
     *
     * @param elemParser      a parser that matches an element of type T
     * @param delimiterParser a parser that matches an element of type D
     * @tparam T the type of elements if successfully parsed
     * @tparam D the type of delimiter elements
     * @return the parser that matches a delimited sequence of elements of type T
     */
    private def delimited[T, D](elemParser: => Parser[T], delimiterParser: Parser[D] = DCalTokenData.Comma): Parser[List[T]] = {
      (elemParser ~ rep(delimiterParser ~> elemParser)).map {
        case t ~ ts => t :: ts
      }
    }

    lazy val module: Parser[DCalAST.Module] = {
      (elem(DCalTokenData.Module) ~> name ~ opt(imports) ~ rep(definition)).map {
        case name ~ importsOpt ~ definitions =>
          DCalAST.Module(
            name = name,
            imports = importsOpt.getOrElse(Nil),
            definitions = definitions,
          )
      }
    }

    lazy val imports: Parser[List[String]] = {
      (elem(DCalTokenData.Import) ~> delimited(name)).map {
        case imports => imports
      }
    }

    lazy val definition: Parser[DCalAST.Definition] = {
      (elem(DCalTokenData.Def) ~> name ~ elem(DCalTokenData.OpenParenthesis) ~ opt(delimited(name)) ~ elem(DCalTokenData.CloseParenthesis) ~ block).map {
        case name ~ _ ~ params ~ _ ~ block =>
          DCalAST.Definition(
            name = name, params = params.getOrElse(Nil), body = block
          )
      }
    }

    lazy val block: Parser[DCalAST.Block] = {
      (elem(DCalTokenData.OpenCurlyBracket) ~> rep(statement) <~ elem(DCalTokenData.CloseCurlyBracket))
        .map { statements =>
          DCalAST.Block(statements)
        }
    }

    lazy val statement: Parser[DCalAST.Statement] = {
      val await = (elem(DCalTokenData.Await) ~> expression).map(expr => DCalAST.Statement.Await(expr))

      val assignPair: Parser[DCalAST.AssignPair] = (name ~ elem(DCalTokenData.Walrus) ~ expression).map {
        case name ~ _ ~ expr => DCalAST.AssignPair(name = name, expression = expr)
      }

      val assignPairs =
        delimited(elemParser = assignPair, delimiterParser = elem(DCalTokenData.DoublePipe)).map {
          pairs => DCalAST.Statement.AssignPairs(pairs)
        }

      val assignmentOp: Parser[DCalAST.AssignmentOp] = acceptMatch("var/let operator", {
        case DCalTokenData.EqualTo => DCalAST.AssignmentOp.EqualTo
        case DCalTokenData.SlashIn => DCalAST.AssignmentOp.SlashIn
      })

      val exprBoundLet = (elem(DCalTokenData.Let) ~> name ~ assignmentOp ~ expression).map {
        case name ~ op ~ expr => DCalAST.Statement.Let(name = name, assignmentOp = op, binding = Right(expr))
      }

      val callBoundLet = (elem(DCalTokenData.Let) ~> name ~ assignmentOp ~ aCall).map {
        case name ~ op ~ call => DCalAST.Statement.Let(name = name, assignmentOp = op, binding = Left(call))
      }

      val `var` =
        (elem(DCalTokenData.Var) ~> name ~ opt(assignmentOp ~ expression)).map {
          case name ~ Some(opOpt ~ exprOpt) => DCalAST.Statement.Var(name = name, expressionOpt = Some((opOpt, exprOpt)))
          case name ~ None => DCalAST.Statement.Var(name = name, expressionOpt = None)
        }

      val ifThenElse =
        (elem(DCalTokenData.If) ~> expression ~ elem(DCalTokenData.Then) ~ block ~ elem(DCalTokenData.Else) ~ block).map {
          case predicate ~ _ ~ thenBlock ~ _ ~ elseBlock => DCalAST.Statement.IfThenElse(
            predicate = predicate, thenBlock = thenBlock, elseBlock = elseBlock
          )
        }

      val call =
        aCall.map {
          case call => DCalAST.Statement.Call(call = call)
        }

      ((await | callBoundLet | exprBoundLet | `var` | assignPairs | call) <~ elem(DCalTokenData.Semicolon)) | ifThenElse
    }

    // TODO: Operator precedence behaviour needs reworking
    lazy val expression: Parser[DCalAST.Expression] =  {
      val unOp: Parser[DCalAST.UnOp] =
        acceptMatch("unOp", {
          case DCalTokenData.Tilda => DCalAST.UnOp.Not
        })

      val expressionUnOp: Parser[DCalAST.Expression.ExpressionUnOp] =
        (unOp ~ expressionBase).map {
          case unOp ~ expr => DCalAST.Expression.ExpressionUnOp(unop = unOp, expr = expr)
        }

      val relOp: Parser[DCalAST.RelOp] =
        acceptMatch("relOp", {
          case DCalTokenData.EqualTo => DCalAST.RelOp.EqualTo
          case DCalTokenData.NotEqualTo => DCalAST.RelOp.NotEqualTo
          case DCalTokenData.LesserThan => DCalAST.RelOp.LesserThan
          case DCalTokenData.LesserThanOrEqualTo => DCalAST.RelOp.LesserThanOrEqualTo
          case DCalTokenData.GreaterThan => DCalAST.RelOp.GreaterThan
          case DCalTokenData.GreaterThanOrEqualTo => DCalAST.RelOp.GreaterThanOrEqualTo
        })

      val expressionRelOp: Parser[DCalAST.Expression.ExpressionRelOp] =
        (expressionBase ~ relOp ~ expressionBase).map {
          case lhs ~ relOp ~ rhs => DCalAST.Expression.ExpressionRelOp(
            lhs = lhs, relOp = relOp, rhs = rhs
          )
        }

      val logicOp: Parser[DCalAST.LogicOp] =
        acceptMatch("logicOp", {
          case DCalTokenData.Or => DCalAST.LogicOp.Or
          case DCalTokenData.And => DCalAST.LogicOp.And
        })

      val expressionLogicOp: Parser[DCalAST.Expression] =
        (expressionBase ~ logicOp ~ expressionBase).map {
          case lhs ~ logicOp ~ rhs => DCalAST.Expression.ExpressionLogicOp(
            lhs = lhs, logicOp = logicOp, rhs = rhs
          )
        }

      val binOp: Parser[DCalAST.BinOp] =
        acceptMatch("binOp", {
          case DCalTokenData.Plus => DCalAST.BinOp.Plus
          case DCalTokenData.Minus => DCalAST.BinOp.Minus
        })

      val expressionBinOp: Parser[DCalAST.Expression] =
        (expressionBase ~ opt(binOp ~ expressionBase)).map {
          case lhs ~ rhsOpt =>
            rhsOpt
              .map {
                case binOp ~ rhs => DCalAST.Expression.ExpressionBinOp(
                  lhs = lhs, binOp = binOp, rhs = rhs
                )
              }
              .getOrElse(lhs)
        }

      expressionUnOp | expressionRelOp | expressionLogicOp | expressionBinOp
    }

    lazy val aCall: Parser[DCalAST.aCall] = {
      val importedName = delimited(name, elem(DCalTokenData.Dot))
      val args = elem(DCalTokenData.OpenParenthesis) ~> opt(delimited(expression)) <~ elem(DCalTokenData.CloseParenthesis)

      (name ~ args).map {
        case defName ~ args => DCalAST.aCall(
          moduleName = Nil,
          definitionName = defName,
          args = args.getOrElse(Nil)
        )
      } | (importedName ~ args).map {
        case (moduleNames :+ defName) ~ args => DCalAST.aCall(
          moduleName = moduleNames, definitionName = defName, args = args.getOrElse(Nil)
        )
      }
    }

    lazy val expressionBase: Parser[DCalAST.Expression] = {
      lazy val boolean = acceptMatch("boolean", {
        case DCalTokenData.True => DCalAST.Expression.True
        case DCalTokenData.False => DCalAST.Expression.False
      })

      val literal = acceptMatch("literal", {
        case DCalTokenData.IntLiteral(v) => DCalAST.Expression.IntLiteral(v)
        case DCalTokenData.StringLiteral(v) => DCalAST.Expression.StringLiteral(v)
      })

      val nameExpr = name.map(str => DCalAST.Expression.Name(str))

      val bracketedExpr =
        (elem(DCalTokenData.OpenParenthesis) ~> expression <~ elem(DCalTokenData.CloseParenthesis)).map { expr => expr }

      val set =
        (elem(DCalTokenData.OpenCurlyBracket) ~> opt(delimited(expression)) <~ elem(DCalTokenData.CloseCurlyBracket)).map { setMembers =>
          DCalAST.Expression.Set(members = setMembers.getOrElse(Nil))
        }

      bracketedExpr | set | boolean | literal | nameExpr
    }
  }

  def apply(contents: String, fileName: String): DCalAST.Module =
    val tokens = DCalTokenizer(contents = contents, fileName = fileName)
    parse(tokens)
}
