package distcompiler.calc

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}
import distcompiler.calc.tokens.*

object CalcParser extends PassSeq:
  import distcompiler.dsl.*
  import Reader.*
  import CalcReader.*

  def inputWellformed: Wellformed = CalcReader.wellformed

  private val mulDivPass = passDef:
    wellformed := inputWellformed.makeDerived:
      Node.Top ::=! repeated(choice(Number, Expression, AddOp, SubOp))
    
    pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          field(tok(Number, Expression))
          ~ skip(tok(MulOp))
          ~ field(tok(Number, Expression))
          ~ trailing
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Mul(
                left.unparent(),
                right.unparent()
              )
            )
          )
        | on(
          field(tok(Number, Expression))
          ~ skip(tok(DivOp))
          ~ field(tok(Number, Expression))
          ~ trailing
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Div(
                left.unparent(),
                right.unparent()
              )
            )
          )

  private val addSubPass = passDef:
    wellformed := prevWellformed.makeDerived:
      Node.Top ::=! repeated(Expression)

    pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          field(tok(Number, Expression))
          ~ skip(tok(AddOp))
          ~ field(tok(Number, Expression))
          ~ trailing
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Add(
                left.unparent(),
                right.unparent()
              )
            )
          )
        | on(
          field(tok(Number, Expression))
          ~ skip(tok(SubOp))
          ~ field(tok(Number, Expression))
          ~ trailing
        ).rewrite: (left, right) =>
          splice(
            Expression(
              Sub(
                left.unparent(),
                right.unparent()
              )
            )
          )
