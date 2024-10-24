package distcompiler.tla

import cats.syntax.all.given
import distcompiler.*
import dsl.*

object TLAParser extends PassSeq:
  import TLAReader.*

  def inputWellformed: Wellformed = TLAReader.wellformed

  val opDeclPattern: SeqPattern[(Node, Int)] =
    // TODO: all the other ones!
    field(Alpha)
    ~ field:
      tok(ParenthesesGroup).withChildren:
        field(repeatedSepBy(`,`)(Alpha.src("_")).map(_.size))
          ~ eof
    ~ trailing

  val unitDefns = passDef:
    wellformed := TLAReader.wellformed.makeDerived:
      Node.Top ::=! repeated(tokens.Module)

      tokens.Id ::= Atom
      tokens.OpSym.importFrom(tla.wellformed)

      val addedTokens = List(
        tokens.Operator,
        tokens.Variable,
        tokens.Constant
      )

      tokens.Module ::= fields(
        tokens.Id,
        tokens.Module.Extends,
        tokens.Module.Defns
      )
      tokens.Module.Extends ::= repeated(tokens.Id)
      tokens.Module.Defns ::= repeated(
        choice(
          TLAReader.ParenthesesGroup.existingCases - TLAReader.`_==_` ++ addedTokens
        )
      )

      TLAReader.groupTokens.foreach: tok =>
        tok.removeCases(TLAReader.`_==_`)
        tok.addCases(addedTokens*)

      tokens.Operator ::= fields(
        choice(tokens.Id, tokens.OpSym),
        choice(TLAReader.ParenthesesGroup, TLAReader.SqBracketsGroup)
      )

      tokens.Variable.importFrom(tla.wellformed)
      tokens.Constant.importFrom(tla.wellformed)
      tokens.OpSym.importFrom(tla.wellformed)

    pass(once = false)
      .rules:
        // module
        on(
          ModuleGroup
          *: children:
            skip(DashSeq)
              ~ skip(optional(Comment))
              ~ skip(Alpha.src("MODULE"))
              ~ skip(optional(Comment))
              ~ field(Alpha)
              ~ skip(optional(Comment))
              ~ skip(DashSeq)
              ~ skip(optional(Comment))
              ~ field(
                optional:
                  skip(Alpha.src("EXTENDS"))
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
              m.unparentedChildren.view.drop(endIdx)
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
            ~ field(tok(Operators.InfixOperator.instances*))
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
            ~ field(tok(Operators.PostfixOperator.instances*))
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
          field(tok(Operators.PrefixOperator.instances*))
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
        // TODO: separate pass to mark keywords
        // variable decls
        | on(
          skip(Alpha.src("VARIABLE") | Alpha.src("VARIABLES"))
            ~ field(repeatedSepBy1(`,`)(Alpha))
            ~ trailing
        ).rewrite: vars =>
          splice(
            vars.map: v =>
              tokens.Variable(tokens.Id().like(v))
          )
        // constant decls
        | on(
          skip(Alpha.src("CONSTANT") | Alpha.src("CONSTANTS"))
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
                      tokens.Order2.Arity(arity.toString)
                    )
                  )
                case (op, arity) =>
                  tokens.Constant(
                    tokens.Order2(
                      op.unparent(),
                      tokens.Order2.Arity(arity.toString)
                    )
                  )
                case alpha: Node =>
                  tokens.Constant(tokens.Id().like(alpha))
          )
