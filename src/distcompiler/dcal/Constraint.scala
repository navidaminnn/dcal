package distcompiler.dcal

import cats.*
import cats.data.*
import cats.syntax.all.given

import distcompiler.util.!!!
import distcompiler.parsing.{SourceLocation, SourceLocated}

enum Constraint[+T] {
  case NameExprAscribe(id: Scoped.NameRef, expr: Scoped.AST.Expression, sourceLocation: SourceLocation) extends Constraint[Unit]
  case NameExprLookup(id: Scoped.NameRef, sourceLocation: SourceLocation) extends Constraint[Scoped.AST.Expression]

  case ExprSetAscribe(id: Scoped.ExprRef, tpe: Constraint.Set, sourceLocation: SourceLocation) extends Constraint[Unit]
  case ExprSetLookup(id: Scoped.ExprRef, sourceLocation: SourceLocation) extends Constraint[Constraint.Set]
  
  case NameSetAscribe(id: Scoped.NameRef, tpe: Constraint.Set, sourceLocation: SourceLocation) extends Constraint[Unit]
  case NameSetLookup(id: Scoped.NameRef, sourceLocation: SourceLocation) extends Constraint[Constraint.Set]

  case Pure(value: T)
  case AndThen[T, U](base: Constraint[T], fn: T => Constraint[U]) extends Constraint[U]
  case Ap[T, U](ff: Constraint[T => U], fa: Constraint[T]) extends Constraint[U]
  case Parallel[T, U](fa1: Constraint[T], fa2: Constraint[U]) extends Constraint[U]

  final def andThen[U](fn: T => Constraint[U]): Constraint[U] =
    AndThen(this, fn)

  final def andThenPar[U](fn: T => Constraint[U]): Constraint[T] =
    this.andThen(t => fn(t) *> t.pure)

  final def seq: Constraint.ConstraintM[T] =
    Constraint.ConstraintM(this)
}

object Constraint {
  import Scoped.{NameRef, ExprRef}

  // TODO: purity checking
  // TODO: effects tracking (assignments, impure calls, asynchrony, and async assignments)
  // TODO: infer invariants needed for sound compilation
  //   ... and where do we check these invariants, and how can we be sure they hold?
  //   ... MVP: don't enforce that invariants are checked, but maybe try to warn
  //  .. try allowing naming assertions / assumptions, and using that to control when they are checked. anonymous = always check

  // idea: the "async" expression. performs the action in an additional thread of execution, splitting the current context.
  //       doesn't get any new copies of vars, changes are atomic though
  // idea: "yield" w/ label breaks atomicity, everything is atomic otherwise

  // idea: check blocks constrain free vars using "override", which takes a pointer to what's being overridden (optional??) and
  //       otherwise matches all contained definitions to abstract outer ones
  //  .. a check block may also inherit shared defs from a struct, and overrides may also be pulled from a struct defn (avoids allowing overrides in any struct)

  final class ConstraintM[+T](val par: Constraint[T]) {
    def flatMap[U](fn: T => ConstraintM[U]): ConstraintM[U] =
      ConstraintM(par.andThen(t => fn(t).par))
  }

  def seq[T](constraintM: ConstraintM[T]): Constraint[T] =
    constraintM.par

  def pure[T](value: T): Constraint[T] =
    Applicative[Constraint].pure(value)

  given monad: StackSafeMonad[ConstraintM] with {
    def flatMap[A, B](fa: ConstraintM[A])(f: A => ConstraintM[B]): ConstraintM[B] =
      fa.flatMap(f)

    def pure[A](x: A): ConstraintM[A] =
      ConstraintM(Constraint.Pure(x))
  }

  given applicative: Applicative[Constraint] with {
    override def productL[A, B](fa: Constraint[A])(fb: Constraint[B]): Constraint[A] =
      Constraint.Parallel(fb, fa)

    override def productR[A, B](fa: Constraint[A])(fb: Constraint[B]): Constraint[B] =
      Constraint.Parallel(fa, fb)

    def ap[A, B](ff: Constraint[A => B])(fa: Constraint[A]): Constraint[B] =
      Constraint.Ap(ff, fa)

    def pure[A](x: A): Constraint[A] =
      Constraint.Pure(x)
  }

  given monoid[T: Monoid]: Monoid[Constraint[T]] with {
    def combine(x: Constraint[T], y: Constraint[T]): Constraint[T] =
      (x, y).mapN(Monoid[T].combine)

    def empty: Constraint[T] =
      Monoid[T].empty.pure
  }

  enum StructDecl extends SourceLocated {
    // TOOD: use NameRef, so we can unambiguously cross-reference field decls (e.g. in effects logic)
    case Var(`abstract`: Boolean, name: String, set: Set)(val sourceLocation: SourceLocation)
    case Let(`abstract`: Boolean, name: String, set: Set)(val sourceLocation: SourceLocation)
    case Def(`abstract`: Boolean, name: String, argSets: List[(String, Set)], resultSet: Set)(val sourceLocation: SourceLocation)
    case Assumption(property: Scoped.AST.Expression)(val sourceLocation: SourceLocation)
    case Assertion(property: Scoped.AST.Expression)(val sourceLocation: SourceLocation)
  }
  
  // maybe we need check vs build (or whatever it's called)
  enum Set {
    case Unknown
    case Atom(name: String)
    case Struct(decls: List[StructDecl])
    case Method(argSets: List[(String, Set)], resultSet: Set)
    case Elements(set: List[Set])
    case Function(segments: List[(Set, Set)])

    def findDecls: Constraint[List[StructDecl]] =
      ???

    def findMethod(name: String, arity: Int)(sourceLocation: SourceLocation): Constraint[StructDecl] =
      ???

    def functionCall(arg: Set)(sourceLocation: SourceLocation): Constraint[Set] =
      ???

    def checkSubsetOf(superSet: Set)(sourceLocation: SourceLocation): Constraint[Unit] =
      ???

    def unify(other: Set)(sourceLocation: SourceLocation): Constraint[Set] =
      ???

    def checkBoolCompat(sourceLocation: SourceLocation): Constraint[Unit] =
      checkSubsetOf(Set.BOOL)(sourceLocation)
  }

  object Set {
    val empty: Set = Set.Elements(Nil)

    val TRUE: Set = Set.Atom("TRUE")
    val FALSE: Set = Set.Atom("FALSE")
    val BOOL: Set = Set.Elements(List(TRUE, FALSE))
  }

  enum Effect {
    case AllocVar(nameRef: NameRef)
    case DupVar(nameRef: NameRef)
    case FreeVar(nameRef: NameRef) // emit on scope end if corresponding AllocVar is present; mark which expr it refers to.
    case WriteVar(nameRef: NameRef)
    case ReadVar(nameRef: NameRef)
    case Call(nameRef: NameRef)
    case Return(set: Set)
    case TailCall(set: Set, nameRef: NameRef) // generate TC if detected (Call last + Return first)
    case Defer(deferredEffects: Effects)
    case Spawn(spawnedEffects: Effects)
  }

  final case class Effects(effects: List[Effect], lastEffects: List[Effect]) {

  }

  def inheritDefns(superDefns: List[StructDecl], defns: List[StructDecl]): Constraint[List[StructDecl]] = ???

  def exprEvalSet(expr: Scoped.AST.Expression): Constraint[Set] = {
    import Scoped.AST.*
    import Scoped.AST.NExpression.*

    expr.extract match {
      case Module(name, body) => ???
      case Seq(first, second) => ???
      case Let(name, setOpt, valueOpt) => ???
      case Var(name, setOpt, valueOpt) => ???
      case Def(name, params, setOpt, bodyOpt) => ???
      case Check(name, body) => ???
      case New(superOpt, body) =>
        superOpt
        .fold(pure(Set.Struct(Nil)))(exprEvalSet)
        .andThen(_.findDecls)
        .andThen { superDecls =>
          superDecls
          .traverse_ {
            case decl @ StructDecl.Var(_, name, set) =>
              Constraint.NameSetAscribe(body.refName(name), set, decl.sourceLocation)
            case decl @ StructDecl.Let(_, name, set) =>
              Constraint.NameSetAscribe(body.refName(name), set, decl.sourceLocation)
            case StructDecl.Def(_, name, argSets, resultSet) => ???
            case _ => pure(())
          }
          *> exprDefns(body)
          .andThen(inheritDefns(superDecls, _))
          .map(Set.Struct.apply)
        }
      case Return(valueOpt) => ???
      case Await(condition) => ???
      case Assert(property) => ???
      case Assume(property) => ???
      case Assignment(pairs) => ???
      case If(predicate, whenTrue, whenFalse) => ???
      case Either(branches) => ???
      case All(branches) => ???
      case Yield(label) => ???
      case Exists(bindings, body) => ???
      case Forall(bindings, body) => ???
      case SubstitutionPoint(name, expr) => ???
      case IntLiteral(value) => ???
      case StringLiteral(value) => ???
      case FunctionApplication(function, argument) => ???
      case Call(name, arguments) =>
        Constraint.NameExprLookup(name.refThisName, name.sourceLocation)
        .andThen { lookupExpr =>
          lookupExpr.extract match {
            case Let(name, setOpt, valueOpt) if arguments.isEmpty => ???
            case Def(name, params, setOpt, bodyOpt) if params.size == arguments.size => ???
            case _ => ???
          }
        }
      case MethodCall(obj, name, arguments) =>
        ???
      case BinaryOpCall(op, lhs, rhs) => ???
      case SetConstructor(members) =>
        members.traverse(exprEvalSet).map(Set.Elements.apply)
      case SetRefinement(binding, body) => ???
      case SetComprehension(body, binds) => ???
      case TupleConstructor(elements) => ???
      case RecordConstructor(fields) => ???
      case RecordSet(fields) => ???
      case Choose(binding, body) => ???
      case FunctionConstructor(bindings, body) => ???
      case FunctionSubstitution(function, substitutions) => ???
    }
  }

  def exprDefns(expr: Scoped.AST.Expression): Constraint[List[StructDecl]] = {
    import Scoped.AST.*
    import Scoped.AST.NExpression.*

    type MkLetVar = (Boolean, Predef.String, Set) => SourceLocation => StructDecl
    def implLetVar(mkLetVar: MkLetVar)(name: String, setOpt: Option[Expression], valueOpt: Option[Expression]): Constraint[Chain[StructDecl]] = {
      val `abstract` = valueOpt.isEmpty
      setOpt
      .fold(pure(Set.Unknown))(exprEvalSet)
      .andThen { set =>
        valueOpt.traverse_ { value =>
          exprSet(value).andThen(_.checkSubsetOf(set)(expr.sourceLocation))
        }
        *> Chain.one(mkLetVar(`abstract`, name.extract, set)(expr.sourceLocation)).pure
      }
    }

    def impl(expr: Scoped.AST.Expression): Constraint[Chain[StructDecl]] =
      expr.extract match {
        case Seq(first, second) =>
          impl(first) `combine` impl(second)
        case Let(name, setOpt, valueOpt) =>
          implLetVar(StructDecl.Let.apply)(name, setOpt, valueOpt)
        case Var(name, setOpt, valueOpt) =>
          implLetVar(StructDecl.Var.apply)(name, setOpt, valueOpt)
        case Def(name, params, setOpt, bodyOpt) =>
          val `abstract` = bodyOpt.isEmpty
          val paramSets =
            params.traverse { (name, setOpt) =>
              setOpt
              .fold(pure(Set.Unknown))(exprEvalSet)
              .andThenPar { set =>
                Constraint.NameSetAscribe(name.refThisName, set, setOpt.fold(name.sourceLocation)(_.sourceLocation))
              }
              .map((name.extract, _))
            }
          val resultSet =
            setOpt.fold(pure(Set.Unknown))(exprEvalSet)
            .andThenPar { set =>
              bodyOpt match {
                case None => ().pure
                case Some(body) =>
                  exprSet(body).andThen(_.checkSubsetOf(set)(expr.sourceLocation))
              }
            }

          (paramSets, resultSet).mapN { (paramSets, resultSet) =>
            Chain.one(StructDecl.Def(`abstract`, name.extract, paramSets, resultSet)(expr.sourceLocation))
          }
        case Assert(property) =>
          exprSet(property)
          *> Chain.one(StructDecl.Assertion(property)(expr.sourceLocation)).pure
        case Assume(property) =>
          exprSet(property)
          *> Chain.one(StructDecl.Assumption(property)(expr.sourceLocation)).pure
        // could allow conditional defs... but only if we can statically eval the predicate
        // case If(predicate, whenTrue, whenFalse) => ???
        case _ =>
          exprSet(expr).andThen(_.checkSubsetOf(Set.empty)(expr.sourceLocation))
          *> Chain.empty.pure
      }

    // TODO: check no redecls
    // TODO: enforce purity (?)
    impl(expr).map(_.toList)
  }

  def exprSet(expr: Scoped.AST.Expression): Constraint[Set] = {
    import Scoped.AST.*
    import Scoped.AST.NExpression.*

    def letVarSet(name: String, setOpt: Option[Expression], valueOpt: Option[Expression]): Constraint[Set] =
      (setOpt, valueOpt) match {
        case (None, None) =>
          Constraint.NameSetAscribe(expr.refName(name.extract), Set.Unknown, expr.sourceLocation)
          *> Set.empty.pure
        case (None, Some(valueExpr)) =>
          exprSet(valueExpr)
          .andThen(Constraint.NameSetAscribe(expr.refName(name.extract), _, expr.sourceLocation))
          *> Set.empty.pure
        case (Some(setExpr), None) =>
          exprEvalSet(setExpr)
          .andThen(Constraint.NameSetAscribe(expr.refName(name.extract), _, expr.sourceLocation))
          *> Set.empty.pure
        case (Some(setExpr), Some(valueExpr)) =>
          exprEvalSet(setExpr)
          .andThenPar(Constraint.NameSetAscribe(expr.refName(name.extract), _, expr.sourceLocation))
          .andThenPar { set =>
            exprSet(valueExpr)
            .andThen(_.checkSubsetOf(set)(setExpr.sourceLocation))
          }
          *> Set.empty.pure
      }

    expr.extract match {
      case Module(name, body) => ???

      case Seq(first, second) =>
        exprSet(first).andThen(_.checkSubsetOf(Set.empty)(first.sourceLocation))
        *> exprSet(second)

      case Let(name, setOpt, valueOpt) =>
        letVarSet(name, setOpt, valueOpt)
      case Var(name, setOpt, valueOpt) =>
        letVarSet(name, setOpt, valueOpt)
      case Def(name, params, setOpt, bodyOpt) => ???

      case Check(name, body) => ???

      case New(superOpt, body) => ???

      case Return(valueOpt) => ???

      case Await(condition) =>
        exprSet(condition)
        .andThen(_.checkBoolCompat(condition.sourceLocation))
        *> Set.empty.pure
      case Assert(property) =>
        exprSet(property)
        .andThen(_.checkBoolCompat(property.sourceLocation))
        *> Set.empty.pure
      case Assume(property) =>
        exprSet(property)
        .andThen(_.checkBoolCompat(property.sourceLocation))
        *> Set.empty.pure
      case Assignment(pairs) =>
        pairs
        .traverse_ { pair =>
          val NAssignPair(lhsBase, lhsIndices, rhs) = pair.extract
          Constraint.NameSetLookup(lhsBase.refThisName, lhsBase.sourceLocation)
          .product(exprSet(rhs))
          .andThen { (lhsBaseSet, rhsSet) =>
            lhsIndices
            .traverse(idx => exprSet(idx).map((_, idx.sourceLocation)))
            .andThen { indexSets =>
              indexSets.foldLeftM(lhsBaseSet)((baseSet, idxPair) => baseSet.functionCall(idxPair._1)(idxPair._2).seq)
              .par
            }
            .andThen(baseSet => rhsSet.checkSubsetOf(baseSet)(pair.sourceLocation))
          }
        }
        *> Set.empty.pure
      case If(predicate, whenTrue, whenFalse) =>
        exprSet(predicate).andThen(_.checkBoolCompat(predicate.sourceLocation))
        *> (exprSet(whenTrue) `product` exprSet(whenFalse))
        .andThen(_.unify(_)(expr.sourceLocation))
      case Either(branches) => ???
      case All(branches) => ???

      case Yield(label) => pure(Set.empty)

      case Exists(bindings, body) => ???
      case Forall(bindings, body) => ???

      case SubstitutionPoint(name, expr) => exprSet(expr)
      case IntLiteral(value) => ???
      case StringLiteral(value) => ???
      case FunctionApplication(function, argument) => ???
      case Call(name, arguments) => ???
      case MethodCall(obj, name, arguments) =>
        exprSet(obj)
        .andThen(_.findMethod(name.extract, arguments.size)(name.sourceLocation))
        .andThen {
          case StructDecl.Let(_, _, set) => set.pure
          case StructDecl.Var(_, _, set) => set.pure
          case StructDecl.Def(_, _, argSets, resultSet) =>
            arguments
            .traverse(exprSet)
            .map(_ `zip` argSets.map(_._2))
            .andThen(_.traverse_(_.checkSubsetOf(_)(name.sourceLocation)))
            *> resultSet.pure
          case _ => !!!
        }
      case BinaryOpCall(op, lhs, rhs) => ???
      case SetConstructor(members) =>
        members.traverse(exprSet).map(Set.Elements.apply)
      case SetRefinement(binding, body) => ???
      case SetComprehension(body, binds) => ???
      case TupleConstructor(elements) =>
        elements
        .traverse(exprSet)
        .map { elementSets =>
          Set.Function {
            elementSets
            .zipWithIndex
            .map { (set, idx) =>
              (Set.Elements(List(Set.Atom(s"i${idx + 1}"))), set)
            }
          }
        }
      case RecordConstructor(fields) =>
        // TODO: uniqueness check
        fields
        .traverse { (name, expr) =>
          exprSet(expr).map((name.extract, _))
        }
        .map { fieldSets =>
          Set.Function(fieldSets.toList.map { (name, set) =>
            (Set.Elements(List(Set.Atom(s"s$name"))), set)
          })
        }
      case RecordSet(fields) => ???
      case Choose(binding, body) => ???
      case FunctionConstructor(bindings, body) => ???
      case FunctionSubstitution(function, substitutions) => ???
    }
  }

  def exprAscribeNames(expr: Scoped.AST.Expression): Constraint[Unit] = ???
}
