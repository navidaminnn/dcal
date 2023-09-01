package distcompiler.dcal

import cats.*
import distcompiler.parsing.Ps
import cats.data.*

import distcompiler.transform.Transformable

object AST {
  enum Import derives Transformable {
    case Name(name: Ps[String])
  }

  // TODO: ability to refine things
  // TODO: which checking block refers to what

  final case class Module(name: Ps[String], imports: List[Ps[Import]], definitions: List[Ps[Definition]]) derives Transformable 

  enum Param derives Transformable {
    case Name(name: Ps[String])
    case Impl(aliasOpt: Option[Ps[String]], interface: Ps[Path])
  }

  enum Definition derives Transformable {
    case Def(qualifierOpt: Option[Ps[Definition.Qualifier]], name: Ps[String], params: List[Ps[Param]], body: Ps[Statement.Block])
    case Check(name: Ps[String], directives: List[Ps[CheckDirective]])
    case Interface(name: Ps[String], supers: List[Ps[Path]], members: List[Ps[InterfaceMember]])
    case Impl(name: Ps[String], params: List[Ps[Param]], interfaces: List[Ps[Path]], definitions: List[Ps[Definition]])
  }

  object Definition {
    enum Qualifier derives CanEqual, Transformable {
      case Pure
      case Async
    }
  }

  enum InterfaceMember derives Transformable {
    case AbstractLet(name: Ps[String])
    case AbstractVar(name: Ps[String])
    case AbstractDef(qualifierOpt: Option[Ps[Definition.Qualifier]], name: Ps[String], params: List[Ps[Param]])
    case ConcreteStatement(stmt: Ps[Statement])
  }

  final case class InstanceDecl(path: Ps[Path], args: List[Ps[CallArg]]) derives Transformable

  enum CallArg derives Transformable {
    case Expr(expression: Ps[Expression])
    case Alias(path: Ps[Path])
    case Instance(decl: Ps[InstanceDecl])
  }

  final case class Path(parts: NonEmptyList[Ps[String]]) derives Transformable

  enum ValBinding derives Transformable {
    case Value(expr: Ps[Expression])
    case Selection(binding: Ps[ValBinding])
    case Call(call: Ps[Statement.Call])
  }

  enum Binding derives Transformable {
    case Val(binding: Ps[ValBinding])
    case Instance(decl: Ps[InstanceDecl])
  }

  enum Statement derives Transformable {
    case Await(condition: Ps[Expression])
    case Assert(property: Ps[Expression])
    case Assume(property: Ps[Expression])
    case Return(value: Ps[ValBinding])
    case Assignment(pairs: NonEmptyList[Ps[AssignPair]])
    case Let(name: Ps[String], binding: Ps[Binding])
    case Var(name: Ps[String], binding: Ps[ValBinding])
    case Block(statements: List[Ps[Statement]])
    case If(predicate: Ps[Expression], thenBlock: Ps[Block], elseBlockOpt: Option[Ps[Block]])
    case Either(blocks: NonEmptyList[Ps[Block]])
    case Call(path: Ps[Path], arguments: List[Ps[CallArg]])
    case Fork(bindings: List[Ps[QuantifierBound]], blocks: NonEmptyList[Ps[Block]])
    case Spawn(bindings: List[Ps[QuantifierBound]], block: Ps[Statement.Block])
    case Defer(label: Ps[String])
    case LocalDefinition(defn: Ps[Definition])
  }

  enum AssignLhs derives Transformable {
    case Name(name: Ps[String])
    case Index(prefix: Ps[AssignLhs], index: Ps[Expression])
  }

  final case class AssignPair(path: Ps[AssignLhs], rhs: Ps[ValBinding])

  enum QuantifierBound derives Transformable {
    case Single(name: Ps[String], set: Ps[Expression])
    case Tuple(names: NonEmptyList[Ps[String]], set: Ps[Expression])
  }

  enum Expression derives Transformable {
    case SubstitutionPoint(name: Ps[String], expr: Ps[Expression])
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case FunctionApplication(function: Ps[Expression], argument: Ps[Expression])
    case OpCall(path: Ps[Path], arguments: List[Ps[Expression]])
    case BinaryOpCall(op: Ps[BinaryOperator], lhs: Ps[Expression], rhs: Ps[Expression])
    //case UnaryOpCall(op: Either[Ps[Tokens.PrefixOperator], Ps[Tokens.PostfixOperator]], operand: Ps[Expression])
    case SetConstructor(members: List[Ps[Expression]])
    case SetRefinement(binding: Ps[QuantifierBound], body: Ps[Expression])
    case SetComprehension(body: Ps[Expression], binds: NonEmptyList[Ps[QuantifierBound]])
    case TupleConstructor(elements: List[Ps[Expression]])
    case RecordConstructor(fields: NonEmptyList[(Ps[String], Ps[Expression])])
    case RecordSet(fields: NonEmptyList[(Ps[String], Ps[Expression])])
    case Exists(bindings: NonEmptyList[Ps[QuantifierBound]], body: Ps[Expression])
    case Forall(bindings: NonEmptyList[Ps[QuantifierBound]], body: Ps[Expression])
    case Choose(binding: Ps[QuantifierBound], body: Ps[Expression])
    case FunctionConstructor(bindings: NonEmptyList[Ps[QuantifierBound]], body: Ps[Expression])
    case FunctionSubstitution(function: Ps[Expression], substitutions: NonEmptyList[Ps[(NonEmptyList[Ps[Expression]], Ps[Expression])]])
  }

  enum CheckDirective derives Transformable {
    case CheckTimeStatement(stmt: Ps[Statement])
    case OverrideLet(path: Ps[Path], expr: Ps[Binding])
    case OverrideVar(path: Ps[Path], expr: Ps[Expression])
  }
}
