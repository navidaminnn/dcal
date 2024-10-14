package distcompiler.tla

import cats.syntax.all.given

import distcompiler.*

object tokens:
  object Module extends Token:
    override val symbolTableFor: Set[Token] =
      Set(Id, OpSym)

    object Extends extends Token
    object Defns extends Token

  object Id extends Token.ShowSource
  object Ids extends Token
  object OpSym extends Token.ShowSource
  object Order2 extends Token:
    override val lookedUpBy: Pattern[Set[Node]] =
      import dsl.*
      tok(Order2) *> children(tok(Id).map(Set(_)))
    object Arity extends Token.ShowSource

  object Expr extends Token:
    object IntLiteral extends Token.ShowSource
    object RationalLiteral extends Token.ShowSource
    object StringLiteral extends Token.ShowSource

    object SetLiteral extends Token
    object TupleLiteral extends Token
    object RecordLiteral extends Token:
      object Field extends Token
    object RecordSetLiteral extends Token:
      object Field extends Token

    object Project extends Token
    object OpCall extends Token:
      object Params extends Token

    object If extends Token

    object Case extends Token:
      object Branch extends Token

    object Let extends Token:
      override val symbolTableFor: Set[Token] =
        Set(Id, OpSym)

      object Defns extends Token

    transparent trait TokenWithQuantifierBounds extends Token:
      override val symbolTableFor: Set[Token] =
        Set(Id)

    object Exists extends TokenWithQuantifierBounds
    object Forall extends TokenWithQuantifierBounds
    object Function extends TokenWithQuantifierBounds
    object SetComprehension extends TokenWithQuantifierBounds
    object SetRefinement extends TokenWithQuantifierBounds
    object Choose extends TokenWithQuantifierBounds

    object Except extends Token:
      object Substitutions extends Token
      object Substitution extends Token:
        override val symbolTableFor: Set[Token] =
          Set(Anchor)
      object Path extends Token:
        override val lookedUpBy: Pattern[Set[Node]] =
          import dsl.*
          tok(Path) *> Pattern.pure(Set(Anchor()))
      object Anchor extends Token

    object Lambda extends Token:
      override val symbolTableFor: Set[Token] =
        Operator.symbolTableFor

      object Params extends Token

  object Operator extends Token:
    override val symbolTableFor: Set[Token] =
      Set(Id, OpSym)
    override val lookedUpBy: Pattern[Set[Node]] =
      import dsl.*
      tok(Operator) *> children(tok(Id, OpSym)).map(Set(_))

    object Params extends Token

  object Variable extends Token:
    override val lookedUpBy: Pattern[Set[Node]] =
      import dsl.*
      tok(Variable) *> children(tok(Id).map(Set(_)))

  object Constant extends Token:
    override val lookedUpBy: Pattern[Set[Node]] =
      import dsl.*
      tok(Constant) *> children:
        tok(Id).map(Set(_))
          | Order2.lookedUpBy

  object QuantifierBound extends Token:
    override val lookedUpBy: Pattern[Set[Node]] =
      import dsl.*
      tok(QuantifierBound)
      *> children:
        tok(Id).map(Set(_))
          | tok(Ids) *> children:
            repeated(tok(Id)).map(_.toSet)

  object QuantifierBounds extends Token

val wellformed: Wellformed =
  import wf.*
  import tokens.*
  Wellformed:
    Node.Top ::= repeated(tok(Module))

    Module ::= fields(
      tok(Id),
      tok(Module.Extends),
      tok(Module.Defns)
    )
    Module.Extends ::= repeated(tok(Id))
    Module.Defns ::= repeated(
      tok(
        Operator,
        Variable,
        Constant
      )
    )

    Id ::= Atom
    Ids ::= repeated(tok(Id), minCount = 1)
    OpSym ::= Atom
    Order2 ::= fields(
      tok(Id),
      tok(Order2.Arity)
    )
    Order2.Arity ::= Atom

    Expr ::= tok(
      Expr.IntLiteral,
      Expr.RationalLiteral,
      Expr.StringLiteral,
      Expr.SetLiteral,
      Expr.TupleLiteral,
      Expr.RecordLiteral,
      Expr.Project,
      Expr.OpCall,
      Expr.If,
      Expr.Case,
      Expr.Let,
      Expr.Exists,
      Expr.Forall,
      Expr.Function,
      Expr.SetComprehension,
      Expr.SetRefinement,
      Expr.Choose,
      Expr.Except,
      Expr.Lambda
    )

    Expr.IntLiteral ::= Atom
    Expr.RationalLiteral ::= Atom
    Expr.StringLiteral ::= Atom

    Expr.SetLiteral ::= repeated(tok(Expr))
    Expr.TupleLiteral ::= repeated(tok(Expr))
    Expr.RecordLiteral ::= repeated(tok(Expr.RecordLiteral.Field), minCount = 1)
    Expr.RecordLiteral.Field ::= fields(
      tok(Id),
      tok(Expr)
    )
    Expr.RecordSetLiteral ::= repeated(
      tok(Expr.RecordLiteral.Field),
      minCount = 1
    )
    Expr.RecordSetLiteral.Field ::= fields(
      tok(Id),
      tok(Expr)
    )

    Expr.OpCall ::= fields(
      tok(Id, OpSym),
      tok(Expr.OpCall.Params)
    )
    Expr.OpCall.Params ::= repeated(tok(Expr))

    Expr.If ::= fields(
      tok(Expr),
      tok(Expr),
      tok(Expr)
    )

    Expr.Case ::= repeated(tok(Expr.Case.Branch), minCount = 1)
    Expr.Case.Branch ::= fields(
      tok(Expr),
      tok(Expr)
    )

    Expr.Let ::= fields(
      tok(Expr.Let.Defns),
      tok(Expr)
    )
    Expr.Let.Defns ::= repeated(
      tok(
        Operator
      ),
      minCount = 1
    )

    Expr.Exists ::= fields(
      tok(QuantifierBounds),
      tok(Expr)
    )
    Expr.Forall ::= fields(
      tok(QuantifierBounds),
      tok(Expr)
    )
    Expr.Function ::= fields(
      tok(QuantifierBounds),
      tok(Expr)
    )
    Expr.SetComprehension ::= fields(
      tok(Expr),
      tok(QuantifierBounds)
    )
    Expr.SetRefinement ::= fields(
      tok(QuantifierBound),
      tok(Expr)
    )
    Expr.Choose ::= fields(
      tok(QuantifierBound),
      tok(Expr)
    )

    Expr.Except ::= fields(
      tok(Expr),
      tok(Expr.Except.Substitutions)
    )
    Expr.Except.Substitutions ::= repeated(
      tok(Expr.Except.Substitution),
      minCount = 1
    )
    Expr.Except.Substitution ::= fields(
      tok(Expr.Except.Path),
      tok(Expr)
    )
    Expr.Except.Path ::= repeated(tok(Expr), minCount = 1)
    Expr.Except.Anchor ::= Atom

    Expr.Lambda ::= fields(
      tok(Expr.Lambda.Params),
      tok(Expr)
    )
    Expr.Lambda.Params ::= repeated(
      tok(
        Id,
        Order2
      ),
      minCount = 1
    )

    Operator ::= fields(
      tok(Id, OpSym),
      tok(Operator.Params),
      tok(Expr)
    )
    Operator.Params ::= repeated(
      tok(
        Id,
        Order2
      )
    )

    Variable ::= fields(tok(Id))
    Constant ::= fields(tok(Id, Order2))

    QuantifierBound ::= fields(
      tok(Id, Ids),
      tok(Expr)
    )

    QuantifierBounds ::= repeated(tok(QuantifierBound), minCount = 1)
