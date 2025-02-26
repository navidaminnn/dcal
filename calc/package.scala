package distcompiler.calc

import cats.syntax.all.given

import scala.collection.mutable

import distcompiler.*
import dsl.*
import distcompiler.calc.tokens.*

object tokens:
  object Expression extends Token:
    override def showSource: Boolean = true

  object AddOp extends Token:
    override def showSource: Boolean = true

  object DivOp extends Token:
    override def showSource: Boolean = true

  object MulOp extends Token:
    override def showSource: Boolean = true

  object SubOp extends Token:
    override def showSource: Boolean = true

  object Add extends Token:
    override def showSource: Boolean = true

  object Sub extends Token:
    override def showSource: Boolean = true

  object Mul extends Token:
    override def showSource: Boolean = true

  object Div extends Token:
    override def showSource: Boolean = true

  object Number extends Token:
    override def showSource: Boolean = true

val wellformed: Wellformed =
  Wellformed:
    Node.Top ::= repeated(choice(tokens.Number, tokens.AddOp, tokens.SubOp, tokens.MulOp, tokens.DivOp))
    
    tokens.Number ::= Atom
    tokens.AddOp ::= Atom
    tokens.SubOp ::= Atom
    tokens.MulOp ::= Atom
    tokens.DivOp ::= Atom

    tokens.Add ::= fields(
      choice(tokens.Number, tokens.Expression),
      choice(tokens.Number, tokens.Expression)
    )

    tokens.Sub ::= fields(
      choice(tokens.Number, tokens.Expression),
      choice(tokens.Number, tokens.Expression)
    )

    tokens.Mul ::= fields(
      choice(tokens.Number, tokens.Expression),
      choice(tokens.Number, tokens.Expression)
    )
    
    tokens.Div ::= fields(
      choice(tokens.Number, tokens.Expression),
      choice(tokens.Number, tokens.Expression)
    )

    tokens.Expression ::= choice(
      tokens.Add,
      tokens.Sub,
      tokens.Mul,
      tokens.Div
    )

object read:
  def fromSourceRange(sourceRange: SourceRange): Node.Top =
    CalcReader(sourceRange)