// Copyright 2024 DCal Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package distcompiler.tla

import cats.syntax.all.given
import distcompiler.*
import dsl.*

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
    override val lookedUpBy: Manip[Set[Node]] =
      on(
        Order2.withChildren(tok(Id).map(Set(_)))
      ).value

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
    object FnCall extends Token

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
        override val lookedUpBy: Manip[Set[Node]] =
          on(Path).check.as(Set(Anchor()))
      object Anchor extends Token

    object Lambda extends Token:
      override val symbolTableFor: Set[Token] =
        Operator.symbolTableFor

      object Params extends Token

  object Operator extends Token:
    override val symbolTableFor: Set[Token] =
      Set(Id, OpSym)
    override val lookedUpBy: Manip[Set[Node]] =
      on(tok(Operator) *> children(tok(Id, OpSym)).map(Set(_))).value

    object Params extends Token

  object Variable extends Token:
    override val lookedUpBy: Manip[Set[Node]] =
      on(tok(Variable) *> children(tok(Id).map(Set(_)))).value

  object Constant extends Token:
    override val lookedUpBy: Manip[Set[Node]] =
      on(
        tok(Constant).withChildren:
          tok(Id).map(Set(_))
            | refine(Order2.lookedUpBy)
      ).value

  object Local extends Token
  object Recursive extends Token:
    override val lookedUpBy: Manip[Set[Node]] =
      on(
        tok(Recursive).withChildren:
          tok(Id).map(Set(_))
            | refine(Order2.lookedUpBy)
      ).value
  object Assumption extends Token
  object Theorem extends Token
  object Instance extends Token:
    object Substitutions extends Token
    object Substitution extends Token

  object QuantifierBound extends Token:
    override val lookedUpBy: Manip[Set[Node]] =
      on(
        tok(QuantifierBound)
        *> children:
          tok(Id).map(Set(_))
            | tok(Ids) *> children:
              repeated(tok(Id)).map(_.toSet)
      ).value

  object QuantifierBounds extends Token

val wellformed: Wellformed =
  import dsl.*
  import tokens.*
  Wellformed:
    Node.Top ::= repeated(Module)

    Module ::= fields(
      Id,
      Module.Extends,
      Module.Defns
    )
    Module.Extends ::= repeated(Id)
    Module.Defns ::= repeated(
      choice(
        Local,
        Recursive,
        Operator,
        Variable,
        Constant,
        Assumption,
        Theorem,
        Instance
      )
    )

    Local ::= choice(
      Operator,
      Instance
      // ModuleDefinition,
    )

    Recursive ::= choice(
      Id,
      Order2
    )

    Id ::= Atom
    Ids ::= repeated(Id, minCount = 1)
    OpSym ::= choice(defns.Operator.instances.toSet)
    defns.Operator.instances.foreach(_ ::= Atom)
    Order2 ::= fields(
      Id,
      embedded[Int]
    )

    Expr ::= choice(
      Expr.IntLiteral,
      Expr.RationalLiteral,
      Expr.StringLiteral,
      Expr.SetLiteral,
      Expr.TupleLiteral,
      Expr.RecordLiteral,
      Expr.Project,
      Expr.OpCall,
      Expr.FnCall,
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

    Expr.SetLiteral ::= repeated(Expr)
    Expr.TupleLiteral ::= repeated(Expr)
    Expr.RecordLiteral ::= repeated(Expr.RecordLiteral.Field, minCount = 1)
    Expr.RecordLiteral.Field ::= fields(
      Id,
      Expr
    )
    Expr.Project ::= fields(
      Expr,
      Id
    )
    Expr.RecordSetLiteral ::= repeated(
      Expr.RecordLiteral.Field,
      minCount = 1
    )
    Expr.RecordSetLiteral.Field ::= fields(
      Id,
      Expr
    )

    Expr.OpCall ::= fields(
      choice(Id, OpSym),
      Expr.OpCall.Params
    )
    Expr.OpCall.Params ::= repeated(Expr)

    Expr.FnCall ::= fields(Expr, Expr)

    Expr.If ::= fields(
      Expr,
      Expr,
      Expr
    )

    Expr.Case ::= repeated(Expr.Case.Branch, minCount = 1)
    Expr.Case.Branch ::= fields(
      Expr,
      Expr
    )

    Expr.Let ::= fields(
      Expr.Let.Defns,
      Expr
    )
    Expr.Let.Defns ::= repeated(
      Operator,
      minCount = 1
    )

    Expr.Exists ::= fields(
      QuantifierBounds,
      Expr
    )
    Expr.Forall ::= fields(
      QuantifierBounds,
      Expr
    )
    Expr.Function ::= fields(
      QuantifierBounds,
      Expr
    )
    Expr.SetComprehension ::= fields(
      Expr,
      QuantifierBounds
    )
    Expr.SetRefinement ::= fields(
      QuantifierBound,
      Expr
    )
    Expr.Choose ::= fields(
      QuantifierBound,
      Expr
    )

    Expr.Except ::= fields(
      Expr,
      Expr.Except.Substitutions
    )
    Expr.Except.Substitutions ::= repeated(
      Expr.Except.Substitution,
      minCount = 1
    )
    Expr.Except.Substitution ::= fields(
      Expr.Except.Path,
      Expr
    )
    Expr.Except.Path ::= repeated(Expr, minCount = 1)
    Expr.Except.Anchor ::= Atom

    Expr.Lambda ::= fields(
      Expr.Lambda.Params,
      Expr
    )
    Expr.Lambda.Params ::= repeated(
      choice(
        Id,
        Order2
      ),
      minCount = 1
    )

    Operator ::= fields(
      choice(Id, OpSym),
      Operator.Params,
      Expr
    )
    Operator.Params ::= repeated(
      choice(
        Id,
        Order2
      )
    )

    Variable ::= Id
    Constant ::= choice(Id, Order2)
    Assumption ::= Expr
    Theorem ::= Expr
    Instance ::= fields(
      Id,
      Instance.Substitutions
    )
    Instance.Substitutions ::= repeated(Instance.Substitution)
    Instance.Substitution ::= fields(
      choice(Id, OpSym),
      Expr
    )

    QuantifierBound ::= fields(
      choice(Id, Ids),
      Expr
    )

    QuantifierBounds ::= repeated(QuantifierBound, minCount = 1)
