package com.github.distcompiler.dcal

import parsing.{SourceLocated, SourceLocation}
import scala.quoted.Expr

object DCalAST {
  final case class Module(name: String, imports: List[String], definitions: List[Definition])(using SourceLocation) extends SourceLocated
  final case class Definition(name: String, params: List[String], body: Statement.Block)(using SourceLocation) extends SourceLocated

  enum Path(using SourceLocation) extends SourceLocated {
    case Name(name: String)(using SourceLocation)
    case Project(prefix: Path, name: String)(using SourceLocation)
    case Index(prefix: Path, index: Expression)(using SourceLocation)
  }

  enum Binding(using SourceLocation) extends SourceLocated {
    case Value(expr: Expression)(using SourceLocation)
    case Selection(binding: Binding)(using SourceLocation)
    case Call(path: Path, arguments: List[Expression])(using SourceLocation)
  }

  enum Statement(using SourceLocation) extends SourceLocated {
    case Await(expression: Expression)(using SourceLocation)
    case Assignment(pairs: List[AssignPair])(using SourceLocation)
    case Let(name: String, binding: Binding)(using SourceLocation)
    case Var(name: String, binding: Binding)(using SourceLocation)
    case Block(statements: List[Statement])(using SourceLocation)
    case If(predicate: Expression, thenBlock: Block, elseBlockOpt: Option[Block])(using SourceLocation)
    case Call(call: Binding.Call)(using SourceLocation)
  }

  final case class AssignPair(path: Path, rhs: Expression)(using SourceLocation) extends SourceLocated

  enum Expression(using SourceLocation) extends SourceLocated {
    case PathRef(path: Path)(using SourceLocation)
    case IntLiteral(value: BigInt)(using SourceLocation)
    case StringLiteral(value: String)(using SourceLocation)
    case OpCall(path: Path, arguments: List[Expression])(using SourceLocation)
    case SetConstructor(members: List[Expression])(using SourceLocation)
  }
}
