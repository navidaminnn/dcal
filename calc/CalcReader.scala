package distcompiler.calc

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}
import Reader.*
import distcompiler.calc.tokens.*

object CalcReader extends Reader:
  override lazy val wellformed = Wellformed:
    Node.Top ::= repeated(choice(Number, AddOp, SubOp, MulOp, DivOp))
    
    Number ::= Atom
    AddOp ::= Atom
    SubOp ::= Atom
    MulOp ::= Atom
    DivOp ::= Atom

  private val digit: Set[Char] = ('0' to '9').toSet
  private val whitespace: Set[Char] = Set(' ', '\n', '\t')

  private lazy val unexpectedEOF: Manip[SourceRange] =
    consumeMatch: m =>
      addChild(Error("unexpected EOF", SourceMarker(m)))
        *> Manip.pure(m)

  protected lazy val rules: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(whitespace):
          extendThisNodeWithMatch(rules)
        .onOneOf(digit):
          numberMode
        .on('+'):
          addChild(AddOp())
            *> rules
        .on('-'):
          addChild(SubOp())
            *> rules
        .on('*'):
          addChild(MulOp())
            *> rules
        .on('/'):
          addChild(DivOp())
            *> rules
        .fallback:
          bytes.selectOne:
            consumeMatch: m =>
              addChild(Error("invalid byte", SourceMarker(m)))
                *> rules
          | consumeMatch: m =>
            on(theTop).check
              *> Manip.pure(m)
          | unexpectedEOF

  private lazy val numberMode: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(digit)(numberMode)
        .fallback:
          consumeMatch: m =>
            m.decodeString().toIntOption match
              case Some(value) =>
                addChild(Number(m))
                  *> rules
              case None =>
                addChild(Error("invalid number format", SourceMarker(m)))
                  *> rules
