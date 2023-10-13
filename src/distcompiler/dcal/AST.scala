package distcompiler.dcal

import cats.*
import cats.data.*
//import cats.syntax.all.given

import distcompiler.transform.Poly
import distcompiler.parsing.Ps

trait AST {
  type Expression
  type FunctionSubstitutionBranch
  type FunctionSubstitutionAnchor
  type AssignPair
  type Punctuation
  type BinaryOperator
  type QuantifierBound
  type String
  type BigInt
  type Node
  type ScopeRoot

  sealed trait NNode {
    def isScopeRoot: Boolean = false
  }

  sealed trait NScopeRoot extends NNode {
    override def isScopeRoot: Boolean = true
  }

  final case class NAssignPair(lhsBase: String, lhsIndices: List[Expression], rhs: Expression) extends NNode

  enum NQuantifierBound extends NNode {
    case Single(name: String, set: Expression)
    case Tuple(names: NonEmptyList[String], set: Expression)
  }

  enum NExpression extends NNode {
    case Module(name: String, body: Expression) extends NExpression, NScopeRoot

    case Seq(first: Expression, second: Expression)

    case Let(name: String, setOpt: Option[Expression], valueOpt: Option[Expression]) extends NExpression, NScopeRoot
    case Var(name: String, setOpt: Option[Expression], valueOpt: Option[Expression]) extends NExpression, NScopeRoot
    case Def(name: String, params: List[(String, Option[Expression])], setOpt: Option[Expression], bodyOpt: Option[Expression]) extends NExpression, NScopeRoot

    case Check(name: String, body: Expression) extends NExpression, NScopeRoot

    case New(superOpt: Option[Expression], body: Expression) extends NExpression, NScopeRoot

    case Return(valueOpt: Option[Expression])

    case Await(condition: Expression)
    case Assert(property: Expression)
    case Assume(property: Expression)
    case Assignment(pairs: NonEmptyList[AssignPair])
    case If(predicate: Expression, whenTrue: Expression, whenFalse: Expression)
    case Either(branches: NonEmptyList[Expression])
    case All(branches: NonEmptyList[Expression])

    case Yield(label: String)

    case Exists(bindings: NonEmptyList[QuantifierBound], body: Expression) extends NExpression, NScopeRoot
    case Forall(bindings: NonEmptyList[QuantifierBound], body: Expression) extends NExpression, NScopeRoot

    case SubstitutionPoint(name: String, expr: Expression)
    case IntLiteral(value: BigInt)
    case StringLiteral(value: String)
    case FunctionApplication(function: Expression, argument: Expression)
    case Call(name: String, arguments: List[Expression])
    case MethodCall(obj: Expression, name: String, arguments: List[Expression])
    case BinaryOpCall(op: BinaryOperator, lhs: Expression, rhs: Expression)
    //case UnaryOpCall(op: Either[Tokens.PrefixOperator, Tokens.PostfixOperator], operand: ExpressionT)
    case SetConstructor(members: List[Expression])
    case SetRefinement(binding: QuantifierBound, body: Expression) extends NExpression, NScopeRoot
    case SetComprehension(body: Expression, binds: NonEmptyList[QuantifierBound]) extends NExpression, NScopeRoot
    case TupleConstructor(elements: List[Expression])
    case RecordConstructor(fields: NonEmptyList[(String, Expression)])
    case RecordSet(fields: NonEmptyList[(String, Expression)])
    case Choose(binding: QuantifierBound, body: Expression) extends NExpression, NScopeRoot
    case FunctionConstructor(bindings: NonEmptyList[QuantifierBound], body: Expression) extends NExpression, NScopeRoot
    case FunctionSubstitution(function: Expression, substitutions: NonEmptyList[FunctionSubstitutionBranch])
  }

  final case class NFunctionSubstitutionBranch(indexPath: NonEmptyList[Expression], body: Expression) extends NNode, NScopeRoot 
}

transparent trait ASTF[F[_]] extends AST {
  type Expression = F[NExpression]
  type FunctionSubstitutionBranch = F[NFunctionSubstitutionBranch]
  type AssignPair = F[NAssignPair]
  type Punctuation = F[distcompiler.dcal.Punctuation]
  type BinaryOperator = F[distcompiler.dcal.BinaryOperator]
  type QuantifierBound = F[NQuantifierBound]
  type String = F[Predef.String]
  type BigInt = F[scala.BigInt]
  type Node = F[NNode]
  type ScopeRoot = F[NScopeRoot]
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
