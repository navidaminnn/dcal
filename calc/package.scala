package distcompiler.calc

import cats.syntax.all.given

import scala.collection.mutable

import distcompiler.*
import dsl.*

object tokens:
  object Operation extends Token

  object Number extends Token:
    override def showSource: Boolean = true

  object Operator extends Token:
    override def showSource: Boolean = true

  object Result extends Token:
    override val symbolTableFor: Set[Token] = 
      Set(Number)

val wellformed: Wellformed =
  Wellformed:
    Node.Top ::= repeated(choice(tokens.Number, tokens.Operator))
    
    tokens.Number ::= Atom
    tokens.Operator ::= Atom
    
    tokens.Operation ::= fields(
      tokens.Number,
      tokens.Operator,
      tokens.Number
    )

object parse:
  def fromSourceRange(sourceRange: SourceRange): Node.Top =
    CalcReader(sourceRange)