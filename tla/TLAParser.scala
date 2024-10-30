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

object TLAParser extends PassSeq:
  import TLAReader.*

  def inputWellformed: Wellformed = TLAReader.wellformed

  val opDeclPattern: SeqPattern[(Node, Int)] =
    val alphaCase =
      field(Alpha)
      ~ field:
        tok(ParenthesesGroup).withChildren:
          field(repeatedSepBy(`,`)(Alpha.src("_")).map(_.size))
            ~ eof
      ~ trailing
    val prefixCase =
      locally:
        field(tok(defns.PrefixOperator.instances*))
          ~ skip(Alpha.src("_"))
          ~ trailing
      .map((_, 1))
    val infixCase =
      locally:
        skip(Alpha.src("_"))
          ~ field(tok(defns.InfixOperator.instances*))
          ~ skip(Alpha.src("_"))
          ~ trailing
      .map((_, 2))
    val postfixCase =
      locally:
        skip(Alpha.src("_"))
          ~ field(tok(defns.PostfixOperator.instances*))
          ~ trailing
      .map((_, 1))

    alphaCase
      | prefixCase
      | infixCase
      | postfixCase

  val reservedWordsAndComments = passDef:
    wellformed := TLAReader.wellformed.makeDerived:
      TLAReader.groupTokens.foreach: tok =>
        tok.addCases(defns.ReservedWord.instances*)
      defns.ReservedWord.instances.iterator
        .filter(!_.isInstanceOf[defns.Operator])
        .foreach(_ ::= Atom)

    val reservedWordMap =
      defns.ReservedWord.instances.iterator
        .map: word =>
          SourceRange.entire(Source.fromString(word.spelling)) -> word
        .toMap

    pass(once = true, strategy = pass.bottomUp)
      .rules:
        on(
          tok(Alpha).filter(node => reservedWordMap.contains(node.sourceRange))
        ).rewrite: word =>
          splice(reservedWordMap(word.sourceRange)().like(word))
        | on(
          TLAReader.Comment
        ).rewrite: _ =>
          splice() // delete it. TODO: maybe try to gather comments?

  val unitDefns = passDef:
    wellformed := prevWellformed.makeDerived:
      Node.Top ::=! repeated(tokens.Module)

      tokens.Id ::= Atom
      tokens.OpSym.importFrom(tla.wellformed)

      val addedTokens = List(
        tokens.Operator,
        tokens.Variable,
        tokens.Constant,
        tokens.Assumption,
        tokens.Theorem,
        tokens.Recursive,
        tokens.Instance
      )
      val removedTokens = List(
        TLAReader.`_==_`,
        defns.VARIABLE,
        defns.VARIABLES,
        defns.CONSTANT,
        defns.CONSTANTS,
        defns.ASSUME,
        defns.AXIOM,
        defns.ASSUMPTION,
        defns.THEOREM,
        defns.INSTANCE
      )

      tokens.Module ::= fields(
        tokens.Id,
        tokens.Module.Extends,
        tokens.Module.Defns
      )
      tokens.Module.Extends ::= repeated(tokens.Id)
      tokens.Module.Defns ::= repeated(
        choice(
          TLAReader.ParenthesesGroup.existingCases -- removedTokens ++ addedTokens
        )
      )

      TLAReader.groupTokens.foreach: tok =>
        tok.removeCases(removedTokens*)
        tok.addCases(addedTokens*)

      tokens.Operator ::= fields(
        choice(tokens.Id, tokens.OpSym),
        choice(TLAReader.ParenthesesGroup, TLAReader.SqBracketsGroup)
      )

      tokens.Variable.importFrom(tla.wellformed)
      tokens.Constant.importFrom(tla.wellformed)
      tokens.OpSym.importFrom(tla.wellformed)
      tokens.Assumption ::= Atom
      tokens.Theorem ::= Atom
      tokens.Recursive.importFrom(tla.wellformed)
      tokens.Instance ::= tokens.Id

    pass(once = true, strategy = pass.bottomUp)
      .rules:
        // module
        on(
          ModuleGroup
          *: children:
            skip(DashSeq)
              ~ skip(optional(Comment))
              ~ skip(defns.MODULE)
              ~ skip(optional(Comment))
              ~ field(Alpha)
              ~ skip(optional(Comment))
              ~ skip(DashSeq)
              ~ skip(optional(Comment))
              ~ field(
                optional:
                  skip(defns.EXTENDS)
                    ~ field(
                      repeatedSepBy1(skip(optional(Comment)) ~ skip(`,`)):
                        skip(optional(Comment))
                          ~ field(Alpha)
                          ~ trailing
                    ) ~ trailing
              ) ~ field(optional(anyChild))
              ~ trailing
        ).rewrite: (m, name, extendsOpt, endOpt) =>
          val exts = extendsOpt match
            case None       => Nil
            case Some(exts) => exts.map(ext => tokens.Id().like(ext))
          val defns = endOpt match
            case None => Nil
            case Some(end) =>
              val endIdx = end.idxInParent
              // .init to remove the EqSeq at end
              m.unparentedChildren.view.drop(endIdx).init
          splice(
            tokens.Module(
              tokens.Id().like(name),
              tokens.Module.Extends(exts),
              tokens.Module.Defns(defns)
            )
          )
        // operator defn variations
        | on(
          field(Alpha)
            ~ field(optional(ParenthesesGroup))
            ~ skip(`_==_`)
            ~ trailing
        ).rewrite: (name, paramsOpt) =>
          splice(
            tokens.Operator(
              tokens.Id().like(name),
              paramsOpt match
                case None         => ParenthesesGroup()
                case Some(params) => params.unparent()
            )
          )
        | on(
          field(Alpha)
            ~ field(tok(defns.InfixOperator.instances*))
            ~ field(Alpha)
            ~ skip(`_==_`)
            ~ trailing
        ).rewrite: (param1, op, param2) =>
          splice(
            tokens.Operator(
              tokens.OpSym(op.unparent()),
              ParenthesesGroup(param1.unparent(), param2.unparent())
            )
          )
        | on(
          field(Alpha)
            ~ field(tok(defns.PostfixOperator.instances*))
            ~ skip(`_==_`)
            ~ trailing
        ).rewrite: (param, op) =>
          splice(
            tokens.Operator(
              tokens.OpSym(op.unparent()),
              ParenthesesGroup(param.unparent())
            )
          )
        | on(
          field(tok(defns.PrefixOperator.instances*))
            ~ field(Alpha)
            ~ skip(`_==_`)
            ~ trailing
        ).rewrite: (op, param) =>
          splice(
            tokens.Operator(
              tokens.OpSym(op.unparent()),
              ParenthesesGroup(param.unparent())
            )
          )
        | on(
          field(Alpha)
            ~ field(SqBracketsGroup)
            ~ skip(`_==_`)
            ~ trailing
        ).rewrite: (name, params) =>
          splice(
            tokens.Operator(
              tokens.Id().like(name),
              params.unparent()
            )
          )
        // variable decls
        | on(
          skip(tok(defns.VARIABLE, defns.VARIABLES))
            ~ field(repeatedSepBy1(`,`)(Alpha))
            ~ trailing
        ).rewrite: vars =>
          splice(
            vars.map: v =>
              tokens.Variable(tokens.Id().like(v))
          )
        // constant decls
        | on(
          skip(tok(defns.CONSTANT, defns.CONSTANTS))
            ~ field(
              repeatedSepBy1(`,`):
                opDeclPattern
                  | tok(Alpha)
            ) ~ trailing
        ).rewrite: decls =>
          splice(
            decls.iterator
              .map:
                case (alpha, arity) if alpha.token == Alpha =>
                  tokens.Constant(
                    tokens.Order2(
                      tokens.Id().like(alpha),
                      Node.Embed(arity)
                    )
                  )
                case (op, arity) =>
                  tokens.Constant(
                    tokens.Order2(
                      op.unparent(),
                      Node.Embed(arity)
                    )
                  )
                case alpha: Node =>
                  tokens.Constant(tokens.Id().like(alpha))
          )
        // assume
        | on(
          tok(defns.ASSUME, defns.ASSUMPTION, defns.AXIOM)
        ).rewrite: kw =>
          splice(tokens.Assumption().like(kw))
        // theorem
        | on(
          tok(defns.THEOREM, defns.PROPOSITION, defns.LEMMA, defns.COROLLARY)
        ).rewrite: kw =>
          splice(tokens.Theorem().like(kw))
        // recursive
        | on(
          skip(tok(defns.RECURSIVE))
            ~ field(
              repeatedSepBy1(`,`):
                opDeclPattern
                  | tok(Alpha)
            )
            ~ trailing
        ).rewrite: decls =>
          splice(
            decls.iterator
              .map:
                case (alpha, arity) if alpha.token == Alpha =>
                  tokens.Recursive(
                    tokens.Order2(
                      tokens.Id().like(alpha),
                      Node.Embed(arity)
                    )
                  )
                case (op, arity) =>
                  tokens.Recursive(
                    tokens.Order2(
                      op.unparent(),
                      Node.Embed(arity)
                    )
                  )
                case alpha: Node =>
                  tokens.Recursive(tokens.Id().like(alpha))
          )
        // instance
        | on(
          skip(defns.INSTANCE)
            ~ field(TLAReader.Alpha)
            ~ trailing
        ).rewrite: name =>
          splice(tokens.Instance(tokens.Id().like(name)))

  val joinCompoundUnits = passDef:
    wellformed := prevWellformed.makeDerived:
      tokens.Module.Defns.removeCases(defns.LOCAL)
      tokens.Module.Defns.addCases(tokens.Local, tokens.ModuleDefinition)
      tokens.Local.importFrom(tla.wellformed)
      TLAReader.groupTokens.foreach: tok =>
        tok.removeCases(defns.LOCAL)
        tok.addCases(tokens.Local, tokens.ModuleDefinition)

      tokens.ModuleDefinition ::= fields(
        choice(tokens.Id, tokens.OpSym),
        TLAReader.ParenthesesGroup,
        tokens.Instance
      )

    pass(once = true, strategy = pass.bottomUp)
      .rules:
        // LOCAL <op>
        on(
          field(tok(defns.LOCAL) <* parent(tokens.Module.Defns))
            ~ field(tok(tokens.Operator, tokens.Instance))
            ~ trailing
        ).rewrite: (local, op) =>
          spliceThen(tokens.Local(op.unparent()).like(local)):
            // try again at this location; you might catch LOCAL op == INSTANCE ...
            continuePass
        // <op> == INSTANCE ...
        | on(
          field(
            tok(tokens.Operator) <* children(
              skip(anyNode)
                ~ skip(not(TLAReader.SqBracketsGroup))
                ~ trailing
            )
          )
            ~ field(tokens.Instance)
            ~ trailing
        ).rewrite: (op, instance) =>
          splice(
            tokens.ModuleDefinition(
              op(tokens.Id, tokens.OpSym).unparent(),
              op(TLAReader.ParenthesesGroup).unparent(),
              instance.unparent()
            )
          )
      // TODO: actually look for LOCAL <op> INSTANCE

  // val consumeDefinitionContents = passDef:
  //   val delimiterSeq = Seq(
  //     tokens.Operator,
  //     tokens.Instance,
  //     tokens.Local,
  //     tokens.Recursive,
  //     tokens.Theorem,
  //     tokens.Assumption,
  //     tokens.Variable,
  //     tokens.Constant,
  //     tokens.ModuleDefinition,
  //     TLAReader.DashSeq,
  //   )
  //   wellformed := prevWellformed.makeDerived:
  //     tokens.Operator ::=! fields(
  //       choice(tokens.Id, tokens.OpSym),
  //       choice(TLAReader.ParenthesesGroup, TLAReader.SqBracketsGroup),
  //       tokens.Expr,
  //     )

  //     tokens.Expr ::= prevWellformed.shapeOf(TLAReader.ParenthesesGroup)
  //     tokens.Expr.removeCases(delimiterSeq*)
  //     TLAReader.groupTokens.foreach: tok =>
  //       tok.removeCases(delimiterSeq*)

  //     TLAReader.LetGroup ::=! tla.wellformed.shapeOf(tokens.Expr.Let.Defns)
  //     tokens.Module.Defns ::=! tla.wellformed.shapeOf(tokens.Module.Defns)
  //     tokens.Assumption ::=! tla.wellformed.shapeOf(tokens.Assumption)
  //     tokens.Theorem ::=! tla.wellformed.shapeOf(tokens.Theorem)
  //     tokens.Instance ::=! fields(
  //       tokens.Id,
  //       tokens.Instance.With,
  //     )
  //     tokens.Instance.With ::= repeated(choice(tokens.Expr.existingCases))

  //   val delimiters = tok(delimiterSeq*)

  //   pass(once = true, strategy = pass.bottomUp)
  //     .rules:
  //       val restChildren = field(repeated(anyChild <* not(delimiters)))
  //       on(
  //         TLAReader.DashSeq // delete all ----
  //       ).rewrite: _ =>
  //         splice()
  //       | on(
  //         fields(
  //           (tok(tokens.Local), onlyChild(tokens.Operator)).tupled
  //             | (tok(tokens.Operator) <* not(parent(tokens.Local))).map(op => (op, op))
  //         )
  //         ~ restChildren
  //         ~ trailing
  //       ).rewrite: (owner, op, contents) =>
  //         op.children += tokens.Expr(contents.iterator.map(_.unparent()))
  //         splice(owner)
  //       | on(
  //         field(tokens.Theorem)
  //         ~ restChildren
  //         ~ trailing
  //       ).rewrite: (thm, contents) =>
  //         thm.children += tokens.Expr(contents.iterator.map(_.unparent()))
  //         splice(thm)
  //       | on(
  //         field(tokens.Assumption)
  //         ~ restChildren
  //         ~ trailing
  //       ).rewrite: (assm, contents) =>
  //         assm.children += tokens.Expr(contents.iterator.map(_.unparent()))
  //         splice(assm)
  //       | on(
  //         fields(
  //           (tok(tokens.Instance) <* not(parent(tokens.ModuleDefinition))).map(inst => (inst, inst))
  //           | tok(tokens.ModuleDefinition).product(children(
  //             skip(tok(tokens.Id, tokens.OpSym))
  //             ~ skip(TLAReader.ParenthesesGroup)
  //             ~ field(tokens.Instance)
  //             ~ eof
  //           ))
  //         )
  //         ~ field(optional(
  //           skip(defns.WITH)
  //           ~ restChildren
  //           ~ trailing
  //         ))
  //         ~ trailing
  //       ).rewrite: (parent, inst, contentsOpt) =>
  //         // TODO: actually handle WITH structure (multiple bindings), maybe in another pass
  //         inst.children += tokens.Instance.With(
  //           contentsOpt.getOrElse(Nil).iterator.map(_.unparent())
  //         )
  //         splice(parent)
