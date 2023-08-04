package com.github.distcompiler.dcal

import cats.derived.*
import cats.Order
import parsing.{Ps, SourceLocation}

object DCalAST {
  enum Import derives Order {
    case Name(name: String)
  }

  final case class Module(name: Ps[String], imports: List[Ps[Import]], definitions: List[Ps[Definition]])

  enum DefParam derives Order {
    case Name(name: String)
  }

  final case class Definition(name: Ps[String], params: List[Ps[DefParam]], body: Ps[Statement.Block])

  enum Path {
    case Name(name: String)
    case Project(prefix: Ps[Path], name: String)
    case Index(prefix: Ps[Path], index: Ps[Expression])
  }

  enum Binding {
    case Value(expr: Ps[Expression])
    case Selection(binding: Ps[Binding])
    case Call(path: Ps[Path], arguments: List[Ps[Expression]])
  }

  enum Statement {
    this match {
      case Assignment(pairs) =>
        require(pairs.nonEmpty)
      case _ =>
    }

    case Await(expression: Ps[Expression])
    case Assignment(pairs: List[Ps[AssignPair]])
    case Let(name: Ps[String], binding: Ps[Binding])
    case Var(name: Ps[String], binding: Ps[Binding])
    case Block(statements: List[Ps[Statement]])
    case If(predicate: Ps[Expression], thenBlock: Ps[Block], elseBlockOpt: Option[Ps[Block]])
    case Call(call: Ps[Binding.Call])
  }

  final case class AssignPair(path: Ps[Path], rhs: Ps[Expression])

  enum Expression {
    this match {
      case OpCall(path, arguments) =>
        path match {
          case Left(_) =>
            require(arguments.size == 2)
          case _ =>
        }
      case _ =>
    }

    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case OpCall(path: Either[Ps[DCalTokenizer.BinaryOperator], Ps[Path]], arguments: List[Ps[Expression]])
    case SetConstructor(members: List[Ps[Expression]])
  }
}
