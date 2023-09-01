package distcompiler.dcal

import cats.*
import cats.data.*
import cats.syntax.all.given

import distcompiler.parsing.{Ps, SourceLocation}
import distcompiler.transform.Transformable

object AST {
  sealed trait NameBinder {
    final def boundNames: NonEmptyList[Ps[PathSegment]] =
      import NonEmptyList.one

      def oneName(name: Ps[String]): NonEmptyList[Ps[PathSegment]] =
        NonEmptyList.one(PathSegment.fromName(name))

      def onePn(pn: Ps[Punctuation]): NonEmptyList[Ps[PathSegment]] =
        NonEmptyList.one(PathSegment.fromPunctuation(pn))

      this match {
        case Import.Name(name) => oneName(name)
        case Param.Name(name) => oneName(name)
        case Param.Impl(aliasOpt, interface) =>
          one(aliasOpt.fold(interface.value.basename)(PathSegment.fromName))
        case Definition.Def(qualifierOpt, name, params, body) => oneName(name)
        case Definition.Check(name, block) => oneName(name)
        case Definition.Interface(name, supers, block) => oneName(name)
        case Definition.Impl(name, params, interfaces, block) => oneName(name)
        case InterfaceMember.AbstractLet(name) => oneName(name)
        case InterfaceMember.AbstractVar(name) => oneName(name)
        case InterfaceMember.AbstractDef(qualifierOpt, name, params) => oneName(name)
        case Statements.Let(name, binding, body) => oneName(name)
        case Statements.Var(name, binding, body) => oneName(name)
        case QuantifierBound.Single(name, set) => oneName(name)
        case QuantifierBound.Tuple(names, set) => names.map(PathSegment.fromName)
        case Expression.SubstitutionPoint(name, expr) => oneName(name)
        case FunctionSubstitutionAnchor(location, body) => onePn(Ps(Punctuation.`@`)(using location))
      }  
  }

  sealed trait PathRef {
    final def refPath: Ps[Path] =
      this match {
        case Param.Impl(aliasOpt, interface) => interface
        case InterfaceRef(path) => path
        case InstanceDecl(path, args) => path
        case CallArg.Alias(path) => path
        case AssignLhs.Name(name) => Path.fromName(name)
        case Expression.OpCall(path, arguments) => path
        case Expression.BinaryOpCall(op, lhs, rhs) => Path.fromPunctuation(op.map(_.asPunctuation))
        case CheckDirective.OverrideLet(path, expr) => path
        case CheckDirective.OverrideVar(path, expr) => path
      }
  }

  sealed trait ScopeRoot

  enum Import extends NameBinder derives Transformable {
    case Name(name: Ps[String])
  }

  // TODO: ability to refine things
  // TODO: which checking block refers to what

  final case class Module(name: Ps[String], imports: List[Ps[Import]], definitions: List[Ps[Definition]]) derives Transformable 

  enum Param extends NameBinder derives Transformable {
    case Name(name: Ps[String])
    case Impl(aliasOpt: Option[Ps[String]], interface: Ps[Path]) extends Param, PathRef
  }

  final case class CheckBlock(directives: List[Ps[CheckDirective]]) extends ScopeRoot derives Transformable

  final case class InterfaceBlock(members: List[Ps[InterfaceMember]]) extends ScopeRoot derives Transformable

  final case class ImplBlock(definitions: List[Ps[Definition]]) extends ScopeRoot derives Transformable

  final case class InterfaceRef(path: Ps[Path]) extends PathRef derives Transformable

  enum Definition extends NameBinder, ScopeRoot derives Transformable {
    case Def(qualifierOpt: Option[Ps[Definition.Qualifier]], name: Ps[String], params: List[Ps[Param]], body: Ps[SingleStatement.Block])
    case Check(name: Ps[String], block: Ps[CheckBlock])
    case Interface(name: Ps[String], supers: List[Ps[InterfaceRef]], block: Ps[InterfaceBlock])
    case Impl(name: Ps[String], params: List[Ps[Param]], interfaces: List[Ps[InterfaceRef]], block: Ps[ImplBlock])
  }

  object Definition {
    enum Qualifier derives CanEqual, Transformable {
      case Pure
      case Async
    }
  }

  enum InterfaceMember derives Transformable {
    case AbstractLet(name: Ps[String]) extends InterfaceMember, NameBinder
    case AbstractVar(name: Ps[String]) extends InterfaceMember, NameBinder
    case AbstractDef(qualifierOpt: Option[Ps[Definition.Qualifier]], name: Ps[String], params: List[Ps[Param]]) extends InterfaceMember, ScopeRoot, NameBinder
    case ConcreteStatements(statements: Ps[Statements])
  }

  final case class InstanceDecl(path: Ps[Path], args: List[Ps[CallArg]]) extends PathRef derives Transformable

  enum CallArg derives Transformable {
    case Expr(expression: Ps[Expression])
    case Alias(path: Ps[Path]) extends CallArg, PathRef
    case Instance(decl: Ps[InstanceDecl])
  }

  final case class Path(parts: NonEmptyList[Ps[PathSegment]]) derives Transformable {
    def basename: Ps[PathSegment] = parts.last
  }

  object Path {
    def fromName(name: Ps[String]): Ps[Path] =
      name.coflatMap(name => Path(NonEmptyList.one(PathSegment.fromName(name))))

    def fromPunctuation(pn: Ps[Punctuation]): Ps[Path] =
      pn.coflatMap(pn => Path(NonEmptyList.one(PathSegment.fromPunctuation(pn))))
  }

  enum PathSegment derives Transformable {
    case Name(name: Ps[String])
    case Punctuation(pn: Ps[distcompiler.dcal.Punctuation])
  }

  object PathSegment {
    def fromName(name: Ps[String]): Ps[PathSegment] =
      name.coflatMap(name => PathSegment.Name(name))

    def fromPunctuation(pn: Ps[distcompiler.dcal.Punctuation]): Ps[PathSegment] =
      pn.coflatMap(pn => PathSegment.Punctuation(pn))
  }

  enum ValBinding derives Transformable {
    case Value(expr: Ps[Expression])
    case Selection(binding: Ps[ValBinding])
    case Call(call: Ps[SingleStatement.Call])
  }

  enum Binding derives Transformable {
    case Val(binding: Ps[ValBinding])
    case Instance(decl: Ps[InstanceDecl])
  }

  enum Statements derives Transformable {
    case Single(statement: Ps[SingleStatement], restOpt: Option[Ps[Statements]])
    case Let(name: Ps[String], binding: Ps[Binding], restOpt: Option[Ps[Statements]]) extends Statements, NameBinder, ScopeRoot
    case Var(name: Ps[String], binding: Ps[ValBinding], restOpt: Option[Ps[Statements]]) extends Statements, NameBinder, ScopeRoot
  }

  enum SingleStatement derives Transformable {
    case Await(condition: Ps[Expression])
    case Assert(property: Ps[Expression])
    case Assume(property: Ps[Expression])
    case Return(value: Ps[ValBinding])
    case Assignment(pairs: NonEmptyList[Ps[AssignPair]])
    case Block(statements: Option[Ps[Statements]]) extends SingleStatement, ScopeRoot
    case If(predicate: Ps[Expression], thenBlock: Ps[Block], elseBlockOpt: Option[Ps[Block]])
    case Either(blocks: NonEmptyList[Ps[Block]])
    case Call(path: Ps[Path], arguments: List[Ps[CallArg]])
    case Fork(bindings: List[Ps[QuantifierBound]], blocks: NonEmptyList[Ps[Block]]) extends SingleStatement, ScopeRoot
    case Spawn(bindings: List[Ps[QuantifierBound]], block: Ps[Block]) extends SingleStatement, ScopeRoot
    case Defer(label: Ps[String])
    case LocalDefinition(defn: Ps[Definition])
  }

  enum AssignLhs derives Transformable {
    case Name(name: Ps[String]) extends AssignLhs, PathRef
    case Index(prefix: Ps[AssignLhs], index: Ps[Expression])
  }

  final case class AssignPair(path: Ps[AssignLhs], rhs: Ps[ValBinding])

  enum QuantifierBound extends NameBinder derives Transformable {
    case Single(name: Ps[String], set: Ps[Expression])
    case Tuple(names: NonEmptyList[Ps[String]], set: Ps[Expression])
  }

  enum Expression derives Transformable {
    case SubstitutionPoint(name: Ps[String], expr: Ps[Expression]) extends Expression, NameBinder
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case FunctionApplication(function: Ps[Expression], argument: Ps[Expression])
    case OpCall(path: Ps[Path], arguments: List[Ps[Expression]]) extends Expression, PathRef
    case BinaryOpCall(op: Ps[BinaryOperator], lhs: Ps[Expression], rhs: Ps[Expression]) extends Expression, PathRef
    //case UnaryOpCall(op: Either[Ps[Tokens.PrefixOperator], Ps[Tokens.PostfixOperator]], operand: Ps[Expression])
    case SetConstructor(members: List[Ps[Expression]])
    case SetRefinement(binding: Ps[QuantifierBound], body: Ps[Expression]) extends Expression, ScopeRoot
    case SetComprehension(body: Ps[Expression], binds: NonEmptyList[Ps[QuantifierBound]]) extends Expression, ScopeRoot
    case TupleConstructor(elements: List[Ps[Expression]])
    case RecordConstructor(fields: NonEmptyList[(Ps[String], Ps[Expression])])
    case RecordSet(fields: NonEmptyList[(Ps[String], Ps[Expression])])
    case Exists(bindings: NonEmptyList[Ps[QuantifierBound]], body: Ps[Expression]) extends Expression, ScopeRoot
    case Forall(bindings: NonEmptyList[Ps[QuantifierBound]], body: Ps[Expression]) extends Expression, ScopeRoot
    case Choose(binding: Ps[QuantifierBound], body: Ps[Expression]) extends Expression, ScopeRoot
    case FunctionConstructor(bindings: NonEmptyList[Ps[QuantifierBound]], body: Ps[Expression]) extends Expression, ScopeRoot
    case FunctionSubstitution(function: Ps[Expression], substitutions: NonEmptyList[Ps[FunctionSubstitutionBranch]])
  }

  final case class FunctionSubstitutionAnchor(location: SourceLocation, body: Ps[Expression]) extends NameBinder
  final case class FunctionSubstitutionBody(anchor: FunctionSubstitutionAnchor) extends ScopeRoot
  final case class FunctionSubstitutionBranch(indexPath: NonEmptyList[Ps[Expression]], body: FunctionSubstitutionBody)

  enum CheckDirective derives Transformable {
    case CheckTimeStatements(statements: Ps[Statements])
    case OverrideLet(path: Ps[Path], expr: Ps[Binding]) extends CheckDirective, PathRef
    case OverrideVar(path: Ps[Path], expr: Ps[Expression]) extends CheckDirective, PathRef
  }
}
