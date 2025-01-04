package distcompiler.calc

import cats.syntax.all.given

import scala.collection.mutable

import distcompiler.*
import dsl.*

object tokens:
  object Atom extends Token:
    override def showSource: Boolean = true

val wellformed: Wellformed =
  Wellformed:
    val listContents = repeated(choice(tokens.Atom))
    Node.Top ::= listContents
    tokens.Atom ::= Atom

object parse:
  def fromSourceRange(sourceRange: SourceRange): Node.Top =
    CalcReader(sourceRange)