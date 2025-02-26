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

    Add ::= fields(
      choice(Number, Expression),
      choice(Number, Expression)
    )

    Sub ::= fields(
      choice(Number, Expression),
      choice(Number, Expression)
    )

    Mul ::= fields(
      choice(Number, Expression),
      choice(Number, Expression)
    )
    
    Div ::= fields(
      choice(Number, Expression),
      choice(Number, Expression)
    )

    Expression ::= choice(
      Add,
      Sub,
      Mul,
      Div
    )

  private val digit: Set[Char] = ('0' to '9').toSet
  private val operator: Set[Char] = Set('+', '-', '*', '/')
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
        .onOneOf(operator):
          operatorMode
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

  private lazy val operatorMode: Manip[SourceRange] =
    commit:
      consumeMatch: m =>
        m.decodeString() match
          case "+" =>
            addChild(
              AddOp()
            )
              *> rules
          case "-" =>
            addChild(
              SubOp()
            )
              *> rules
          case "*" =>
            addChild(
              MulOp()
            )
              *> rules
          case "/" =>
            addChild(
              DivOp()
            )
              *> rules
