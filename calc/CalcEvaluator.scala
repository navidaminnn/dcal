package distcompiler.calc

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}
import distcompiler.calc.tokens.*

object CalcEvaluator extends PassSeq:
  import distcompiler.dsl.*
  import Reader.*
  import CalcReader.*

  def inputWellformed: Wellformed = CalcParser.outputWellformed

  private val simplifyPass = passDef:
    wellformed := inputWellformed.makeDerived:
      Node.Top ::=! fields(Number)

    pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          field(tok(Expression)) *> children(
            tok(Add).product(children(
              field(tok(Number))
              ~ field(tok(Number))
              ~ eof
            ))
          )
        ).rewrite: (expression, numbers) =>
          val (left, right) = numbers
          expression.unparent()
          val leftNum = left.unparent().sourceRange.decodeString().toInt
          val rightNum = right.unparent().sourceRange.decodeString().toInt

          splice(
            Number(
              (leftNum + rightNum).toString()
            )
          )
        | on(
          field(tok(Expression)) *> children(
            tok(Sub).product(children(
              field(tok(Number))
              ~ field(tok(Number))
              ~ eof
            ))
          )
        ).rewrite: (expression, numbers) =>
          val (left, right) = numbers
          expression.unparent()
          val leftNum = left.unparent().sourceRange.decodeString().toInt
          val rightNum = right.unparent().sourceRange.decodeString().toInt

          splice(
            Number(
              (leftNum - rightNum).toString()
            )
          )
        | on(
          field(tok(Expression)) *> children(
            tok(Mul).product(children(
              field(tok(Number))
              ~ field(tok(Number))
              ~ eof
            ))
          )
        ).rewrite: (expression, numbers) =>
          val (left, right) = numbers
          expression.unparent()
          val leftNum = left.unparent().sourceRange.decodeString().toInt
          val rightNum = right.unparent().sourceRange.decodeString().toInt

          splice(
            Number(
              (leftNum * rightNum).toString()
            )
          )
        | on(
          field(tok(Expression)) *> children(
            tok(Div).product(children(
              field(tok(Number))
              ~ field(tok(Number))
              ~ eof
            ))
          )
        ).rewrite: (expression, numbers) =>
          val (left, right) = numbers
          expression.unparent()
          val leftNum = left.unparent().sourceRange.decodeString().toInt
          val rightNum = right.unparent().sourceRange.decodeString().toInt

          splice(
            Number(
              (leftNum / rightNum).toString()
            )
          )
