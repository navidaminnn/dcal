package distcompiler.tla

import cats.syntax.all.given
import distcompiler.*

object TLAParser:
  import dsl.*

  // TODO: figure out a general structure for passes
  val unitDefnsWf = TLAReader.wellformed.makeDerived:
    Node.Top ::=! repeated(tokens.Module)

    tokens.Id ::= Atom
    tokens.OpSym ::= Atom

    tokens.Module ::= fields(
      tokens.Id,
      tokens.Module.Extends,
      tokens.Module.Defns
    )
    tokens.Module.Extends ::= repeated(tokens.Id)
    tokens.Module.Defns ::= repeated(
      choice(
        TLAReader.ParenthesesGroup.existingCases - TLAReader.`_==_` + tokens.Operator
      )
    )

    TLAReader.groupTokens.foreach: tok =>
      tok.removeCases(TLAReader.`_==_`)
      tok.addCases(tokens.Operator)

    tokens.Operator ::= fields(
      choice(tokens.Id, tokens.OpSym),
      TLAReader.ParenthesesGroup
    )

  val groupUnitDefns =
    import TLAReader.*
    pass(once = false)
      .rules:
        // module
        on(
          tok(ModuleGroup)
          *: children:
            Fields()
              .skip(tok(DashSeq))
              .optionalSkip(tok(Comment))
              .skip(tok(Alpha).filterSrc("MODULE"))
              .optionalSkip(tok(Comment))
              .field(tok(Alpha))
              .optionalSkip(tok(Comment))
              .skip(tok(DashSeq))
              .optionalSkip(tok(Comment))
              .optionalFields:
                Fields()
                  .skip(tok(Alpha).filterSrc("EXTENDS"))
                  .optionalSkip(tok(Comment))
                  .field(tok(Alpha))
                  .repeatedFields:
                    Fields()
                      .optionalSkip(tok(Comment))
                      .skip(tok(`,`))
                      .optionalSkip(tok(Comment))
                      .field(tok(Alpha))
              .optional(anyChild)
              .trailing
        ).rewrite: (m, name, extendsOpt, endOpt) =>
          val exts = extendsOpt match
            case None => Nil
            case Some((first, rest)) =>
              tokens.Id().like(first) :: rest.map(p => tokens.Id().like(p._1))
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
          Fields()
            .field(tok(Alpha))
            .optional(tok(ParenthesesGroup))
            .skip(tok(`_==_`))
            .trailing
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
          Fields()
            .field(tok(Alpha))
            .field(oneOfToks(Operators.InfixOperator.instances))
            .field(tok(Alpha))
            .skip(tok(`_==_`))
            .trailing
        ).rewrite: (param1, op, param2) =>
          splice(
            tokens.Operator(
              tokens.OpSym(op.unparent()),
              ParenthesesGroup(param1.unparent(), param2.unparent())
            )
          )
        | on(
          Fields()
            .field(tok(Alpha))
            .field(oneOfToks(Operators.PostfixOperator.instances))
            .skip(tok(`_==_`))
            .trailing
        ).rewrite: (param, op) =>
          splice(
            tokens.Operator(
              tokens.OpSym(op.unparent(), ParenthesesGroup(param.unparent())),
              ParenthesesGroup()
            )
          )
        | on(
          Fields()
            .field(oneOfToks(Operators.PrefixOperator.instances))
            .field(tok(Alpha))
            .skip(tok(`_==_`))
            .trailing
        ).rewrite: (op, param) =>
          splice(
            tokens.Operator(
              tokens.OpSym(op.unparent()),
              ParenthesesGroup(param.unparent())
            )
          )

  // TODO: do this properly
  def apply(top: Node.Top, tracer: Manip.Tracer = Manip.NopTracer): Unit =
    atNode(top)(groupUnitDefns *> unitDefnsWf.markErrorsPass)
      .withTracer(tracer)
      .perform()
