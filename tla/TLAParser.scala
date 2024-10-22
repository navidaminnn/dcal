package distcompiler.tla

import cats.syntax.all.given
import distcompiler.*
import dsl.*

object TLAParser extends PassSeq:
  import TLAReader.*

  def inputWellformed: Wellformed = TLAReader.wellformed

  val opDeclFields: Fields[Tuple1[(Node, Int)]] =
    Fields()
      .field(Alpha)
      .field:
        tok(ParenthesesGroup).withChildren:
          Fields()
            .optionalFields:
              Fields()
                .skip(Alpha.filterSrc("_"))
                .repeatedFields:
                  Fields()
                    .skip(`,`)
                    .skip(Alpha.filterSrc("_"))
            .atEnd
      .map:
        case (name, opt) =>
          Tuple1((name, opt._1.fold(0)(_._1.size)))

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
            Fields()
              .skip(DashSeq)
              .optionalSkip(Comment)
              .skip(Alpha.filterSrc("MODULE"))
              .optionalSkip(Comment)
              .field(Alpha)
              .optionalSkip(Comment)
              .skip(DashSeq)
              .optionalSkip(Comment)
              .optionalFields:
                Fields()
                  .skip(Alpha.filterSrc("EXTENDS"))
                  .optionalSkip(Comment)
                  .field(Alpha)
                  .repeatedFields:
                    Fields()
                      .optionalSkip(Comment)
                      .skip(`,`)
                      .optionalSkip(Comment)
                      .field(Alpha)
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
            .field(Alpha)
            .optional(ParenthesesGroup)
            .skip(`_==_`)
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
            .field(Alpha)
            .field(oneOfToks(Operators.InfixOperator.instances))
            .field(Alpha)
            .skip(`_==_`)
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
            .field(Alpha)
            .field(oneOfToks(Operators.PostfixOperator.instances))
            .skip(`_==_`)
            .trailing
        ).rewrite: (param, op) =>
          splice(
            tokens.Operator(
              tokens.OpSym(op.unparent()),
              ParenthesesGroup(param.unparent())
            )
          )
        | on(
          Fields()
            .field(oneOfToks(Operators.PrefixOperator.instances))
            .field(Alpha)
            .skip(`_==_`)
            .trailing
        ).rewrite: (op, param) =>
          splice(
            tokens.Operator(
              tokens.OpSym(op.unparent()),
              ParenthesesGroup(param.unparent())
            )
          )
        | on(
          Fields()
            .field(Alpha)
            .field(SqBracketsGroup)
            .skip(`_==_`)
            .trailing
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
          Fields()
            .skip(Alpha.filterSrc("VARIABLE") | Alpha.filterSrc("VARIABLES"))
            .field(Alpha)
            .repeatedFields:
              Fields()
                .skip(`,`)
                .field(Alpha)
            .trailing
        ).rewrite: (v1, vRest) =>
          splice(
            tokens.Variable(tokens.Id().like(v1))
              :: vRest.map(v => tokens.Variable(tokens.Id().like(v._1)))
          )
        // constant decls
        | on(
          Fields()
            .skip(Alpha.filterSrc("CONSTANT") | Alpha.filterSrc("CONSTANTS"))
            .nestedFields(opDeclFields)
            .repeatedFields:
              Fields()
                .skip(`,`)
                .nestedFields(opDeclFields)
            .trailing
        ).rewrite: (declsHd, declsTl) =>
          splice(
            (Iterator.single(declsHd) ++ declsTl.iterator.map(_._1))
              .map: (name, width) =>
                val id = name.token match
                  case Alpha => tokens.Id().like(name)
                  case op    => name

                if width == 0
                then tokens.Constant(id)
                else
                  tokens.Constant(
                    tokens.Order2(id, tokens.Order2.Arity(width.toString))
                  )
          )
