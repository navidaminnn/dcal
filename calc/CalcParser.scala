package distcompiler.calc

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}

object CalcParser extends PassSeq:
  import distcompiler.dsl.*
  import distcompiler.Builtin.{Error, SourceMarker}
  import Reader.*

  def inputWellformed: Wellformed = CalcReader.wellformed

  private lazy val mulDivPass = passDef:
    wellformed := prevWellformed.makeDerived:
      Node.Top ::=! repeated(choice(tokens.Operation, tokens.Number))

    pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          tok(tokens.Operation).filter(node => 
            Set("*", "/").contains(node.sourceRange.decodeString().trim)
          ).withChildren(
            field(tokens.Number)
              ~ field(tokens.Operator) 
              ~ field(tokens.Number)
              ~ trailing
          )
        ).rewrite: (left, op, right) =>
          val l = left.sourceRange.decodeString().toInt
          val r = right.sourceRange.decodeString().toInt
          val result = op.sourceRange.decodeString().trim match
            case "*" => l * r
            case "/" => 
              if r == 0 then throw ArithmeticException("Division by zero")
              l / r
          
          splice(tokens.Number(result.toString))

  private lazy val addSubPass = passDef:
    wellformed := prevWellformed.makeDerived:
      Node.Top ::=! fields(tokens.Number)

    pass(once = false, strategy = pass.topDown)
      .rules:
        on(
          tok(tokens.Operation).filter(node =>
            Set("+", "-").contains(node.sourceRange.decodeString().trim)  
          ).withChildren(
            field(tokens.Number)
              ~ field(tokens.Operator)
              ~ field(tokens.Number)
              ~ trailing
          )
        ).rewrite: (left, op, right) =>
          val l = left.sourceRange.decodeString().toInt
          val r = right.sourceRange.decodeString().toInt
          val result = op.sourceRange.decodeString().trim match
            case "+" => l + r
            case "-" => l - r
          
          splice(tokens.Number(result.toString))