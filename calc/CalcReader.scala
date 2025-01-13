package distcompiler.calc

import cats.syntax.all.given

import distcompiler.*
import dsl.*
import distcompiler.Builtin.{Error, SourceMarker}
import Reader.*

object CalcReader extends Reader:
  override lazy val wellformed = Wellformed:
    Node.Top ::= repeated(choice(tokens.Number, tokens.Operator))
    
    tokens.Number ::= Atom
    tokens.Operator ::= Atom
    
    tokens.Operation ::= fields(
      tokens.Number,
      tokens.Operator,
      tokens.Number
    )

  private val digit: Set[Char] = ('0' to '9').toSet
  private val operation: Set[Char] = Set('*', '/', '+', '-')
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
        .onOneOf(operation):
          operationMode
        .fallback:
          bytes.selectCount(1):
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
                addChild(tokens.Number(m))
                  *> rules
              case None =>
                addChild(Error("invalid number format", SourceMarker(m)))
                  *> rules

  private lazy val operationMode: Manip[SourceRange] =
    commit:
      consumeMatch: m =>
        addChild(tokens.Operator(m))
          *> rules