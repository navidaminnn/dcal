package distcompiler.dcal

import cats.*
import cats.data.*
//import cats.syntax.all.given

import distcompiler.transform.Poly
import distcompiler.parsing.{Ps, SourceLocation}

trait AST {
  type Expression
  type FunctionSubstitutionBranch
  type FunctionSubstitutionAnchor
  type DefinitionQualifier
  type AssignPair
  type Punctuation
  type BinaryOperator
  type String
  type BigInt

  sealed trait NNode

  sealed trait NScopeRoot

  enum NDefinitionQualifier extends NNode {
    case Pure, Async
  }

  final case class NAssignPair(lhsBase: String, lhsIndices: List[Expression], rhs: Expression) extends NNode

  enum NQuantifierBound extends NNode {
    case Single(name: String, set: Expression)
    case Tuple(names: NonEmptyList[String], set: Expression)
  }

  enum NExpression extends NNode {
    case Module(name: String, body: Expression) extends NScopeRoot, NExpression

    case Seq(first: Expression, second: Expression)

    case Let(name: String, valueOpt: Option[Expression]) extends NScopeRoot, NExpression
    case Var(name: String, valueOpt: Option[Expression]) extends NScopeRoot, NExpression
    case Def(qualifierOpt: Option[DefinitionQualifier], name: String, params: List[ParamT], bodyOpt: Option[Expression]) extends NScopeRoot, NExpression

    case Check(name: String, body: Expression) extends NScopeRoot, NExpression
    case Override(existing: Expression, replacement: Expression)

    case Interface(body: Expression) extends NScopeRoot, NExpression
    case Struct(body: Expression) extends NScopeRoot, NExpression

    case Return(valueOpt: Option[Expression])

    case Await(condition: Expression)
    case Assert(property: Expression)
    case Assume(property: Expression)
    case Assignment(pairs: NonEmptyList[AssignPair])
    case If(predicate: Expression, whenTrue: Expression, whenFalse: Expression)
    case Either(branches: NonEmptyList[Expression])
    case All(branches: NonEmptyList[Expression])

    case Later(label: String, next: Expression)

    case Exists(bindings: NonEmptyList[QuantifierBound], body: Expression) extends NScopeRoot, NExpression
    case Forall(bindings: NonEmptyList[QuantifierBound], body: Expression) extends NScopeRoot, NExpression

    case SubstitutionPoint(name: String, expr: Expression)
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case FunctionApplication(function: Expression, argument: Expression)
    case Call(name: String, arguments: List[Expression])
    case MethodCall(obj: Expression, name: String, arguments: List[Expression])
    case BinaryOpCall(op: BinaryOperator, lhs: Expression, rhs: Expression)
    //case UnaryOpCall(op: Either[Tokens.PrefixOperator, Tokens.PostfixOperator], operand: ExpressionT)
    case SetConstructor(members: List[Expression])
    case SetRefinement(binding: QuantifierBound, body: Expression) extends MScopeRoot, NExpression
    case SetComprehension(body: Expression, binds: NonEmptyList[QuantifierBound]) extends NScopeRoot, NExpression
    case TupleConstructor(elements: List[Expression])
    case RecordConstructor(fields: NonEmptyList[(String, Expression)])
    case RecordSet(fields: NonEmptyList[(String, Expression)])
    case Choose(binding: QuantifierBoundT, body: ExpressionT) extends NScopeRoot, NExpression
    case FunctionConstructor(bindings: NonEmptyList[QuantifierBound], body: Expression) extends NScopeRoot, NExpression
    case FunctionSubstitution(function: Expression, substitutions: NonEmptyList[FunctionSubstitutionBranch])
  }

  final case class NFunctionSubstitutionBranch(indexPath: NonEmptyList[ExpressionT], body: Expression) extends NScopeRoot, NNode
}

transparent trait ASTF[F[_]] extends AST {
  type NodeT = F[Node]
  type StringT = F[String]
  type BigIntT = F[BigInt]
  type PunctuationT = F[Punctuation]
  type BinaryOperatorT = F[BinaryOperator]
  type NameBinderT = F[NameBinder]
  type PathRefT = F[PathRef]
  type ScopeRootT = F[ScopeRoot]
  type ImportT = F[Import]
  type ModuleT = F[Module]
  type ParamT = F[Param]
  type CheckBlockT = F[CheckBlock]
  type InterfaceBlockT = F[InterfaceBlock]
  type ImplBlockT = F[ImplBlock]
  type InterfaceRefT = F[InterfaceRef]
  type DefinitionT = F[Definition]
  type DefinitionQualifierT = F[Definition.Qualifier]
  type InterfaceMemberT = F[InterfaceMember]
  type InstanceDeclT = F[InstanceDecl]
  type CallArgT = F[CallArg]
  type PathT = F[Path]
  type PathSegmentT = F[PathSegment]
  type ValBindingT = F[ValBinding]
  type BindingT = F[Binding]
  type StatementsT = F[Statements]
  type BlockT = F[SingleStatement.Block]
  type SingleStatementT = F[SingleStatement]
  type AssignLhsT = F[AssignLhs]
  type AssignPairT = F[AssignPair]
  type QuantifierBoundT = F[QuantifierBound]
  type ExpressionT = F[Expression]
  type FunctionSubstitutionAnchorT = F[FunctionSubstitutionAnchor]
  type FunctionSubstitutionBodyT = F[FunctionSubstitutionBody]
  type FunctionSubstitutionBranchT = F[FunctionSubstitutionBranch]
  type CheckDirectiveT = F[CheckDirective]
}

object IdAST extends ASTF[Id] {
  def stripFrom[F[_]: Comonad](otherAST: ASTF[F])(otherNode: otherAST.Node): Node = {
    object impl extends Poly, Poly.Identity, Poly.GenericTransform, Poly.FunctorTransform {
      given wrapId[T1, T2](using subCase: =>Case[T1, T2]): Case[T1, Id[T2]] with {
        def apply(t: T1): T2 = subCase(t)
      }

      given comonadExtract[T1, T2](using subCase: =>Case[T1, T2]): Case[F[T1], T2] with {
        def apply(t: F[T1]): T2 = subCase(Comonad[F].extract(t))
      }
    }
    
    impl(otherNode)
  }
}

object ParsedAST extends ASTF[Ps]
