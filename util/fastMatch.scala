// Copyright 2024-2025 DCal Team
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

package distcompiler.util

import scala.quoted.{Type, Quotes, Expr}
import scala.reflect.Enum
import scala.annotation.switch

extension [T <: Enum, U](inline self: T)
  inline def fastMatch(inline fn: Function[T, U]): U =
    ${ fastMatchImpl('self, 'fn) }

private def fastMatchImpl[T <: Enum: Type, U: Type](
    valExpr: Expr[T],
    fnExpr: Expr[Function[T, U]]
)(using quotes: Quotes): Expr[U] =
  import quotes.reflect.*

  object StripInlined:
    def unapply(term: Term): Some[(Term => Term, Term)] =
      term match
        case Inlined(x, y, StripInlined(fn, term)) =>
          Some((t => Inlined(x, y, fn(t)), term))
        case _ => Some((identity, term))

  object StripBind:
    def unapply(tree: Tree): Some[Tree] =
      tree match
        case Bind(_, StripBind(tree)) => Some(tree)
        case _                        => Some(tree)

  Term.betaReduce('{ $fnExpr($valExpr) }.asTerm) match
    case None => report.errorAndAbort("could not reduce operands")
    case Some(StripInlined(restoreInlined, Match(expr, cases))) =>
      val tRepr = TypeRepr.of[T]
      if expr.tpe <:< tRepr
      then
        def getOrdinal(sym: Symbol, pos: Position): Int =
          val ordinal = tRepr.typeSymbol.children.indexOf(sym)
          if ordinal == -1
          then
            report.errorAndAbort(
              s"$sym is not in (${tRepr.typeSymbol.children.mkString(", ")})",
              pos
            )
          ordinal

        def dummyMatch(cse: CaseDef): Term =
          Match('{ ${ expr.asExprOf[T] }: @unchecked }.asTerm, List(cse))

        val branches = cases.map:
          case cse @ CaseDef(
                StripBind(TypedOrTest(Unapply(fun, _, _), _)),
                None,
                _
              ) =>
            val tpeSym = fun.symbol.owner.companionClass
            CaseDef(
              Literal(IntConstant(getOrdinal(tpeSym, fun.pos))),
              None,
              dummyMatch(cse)
            )
          case cse @ CaseDef(StripBind(typed @ Typed(_, tpeTree)), None, _) =>
            tpeTree.tpe.classSymbol match
              case None =>
                report.errorAndAbort(
                  s"can't get class symbol of ${tpeTree.tpe.show}",
                  typed.pos
                )
              case Some(tpeSym) =>
                CaseDef(
                  Literal(IntConstant(getOrdinal(tpeSym, typed.pos))),
                  None,
                  dummyMatch(cse)
                )
          case cse @ CaseDef(StripBind(id @ Ident(_)), None, _) =>
            CaseDef(
              Literal(IntConstant(getOrdinal(id.tpe.termSymbol, id.pos))),
              None,
              dummyMatch(cse)
            )
          case CaseDef(head, _, _) =>
            report.errorAndAbort(
              s"${head.show} can't be interpreted as a case unapply, ident, or type test",
              head.pos
            )
        restoreInlined(
          Match('{ ${ expr.asExprOf[T] }.ordinal: @switch }.asTerm, branches)
        ).asExprOf[U]
      else
        report.errorAndAbort(
          s"${expr.tpe.show} is not a subtype of ${TypeRepr.of[T].show}"
        )
    case _ => report.errorAndAbort("didn't reduce to a match expression")
