package distcompiler.sexpr

import cats.syntax.all.given

import distcompiler.{Reader, Node, Manip}
import distcompiler.SourceRange
import java.nio.CharBuffer

object SExprReader extends Reader:
  import distcompiler.dsl.*
  import distcompiler.Builtin.{Error, SourceMarker}
  import Reader.*

  private val alpha: Set[Byte] =
    (('a' to 'z') ++ ('A' to 'Z'))
      .map(_.toByte)
      .toSet
  private val digit: Set[Byte] =
    ('0' to '9')
      .map(_.toByte)
      .toSet
  private val whitespace: Set[Byte] = Set(' ', '\n', '\r', '\t')

  protected val rules: Manip[SourceRange] =
    bytes
      .selecting[SourceRange]
      .on(whitespace):
        extendThisNodeWithMatch(rules)
      .on('('):
        addChild(tokens.List())
          .here(extendThisNodeWithMatch(rules))
      .on(')'):
        extendThisNodeWithMatch:
          atParent:
            commit(rules)
        | consumeMatch: m =>
          addChild(Error("unexpected end of list", SourceMarker().at(m)))
            *> rules
      .on(digit):
        bytes.selectManyLike(digit):
          consumeMatch: m =>
            m.decodeString().toIntOption match
              case None =>
                addChild(
                  Error(
                    "length doesn't fit in a machine int",
                    SourceMarker().at(m)
                  )
                )
                  *> rules
              case Some(lengthPrefix) =>
                bytes
                  .selecting[SourceRange]
                  .on(':'):
                    dropMatch:
                      bytes.selectCount(lengthPrefix):
                        consumeMatch: m =>
                          addChild(tokens.Atom().at(m))
                            *> rules
                    | bytes.getSrc.flatMap: src =>
                      val srcEnd = src.drop(src.length)
                      addChild(
                        Error(
                          "unexpected EOF before end of raw atom",
                          SourceMarker().at(srcEnd)
                        )
                      )
                        *> Manip.pure(srcEnd)
                  .fallback:
                    addChild(
                      Error(
                        "expected : after length prefix",
                        SourceMarker().at(m)
                      )
                    )
                      *> rules
      .fallback:
        bytes.selectCount(1):
          consumeMatch: m =>
            addChild(Error("invalid byte", SourceMarker().at(m)))
              *> rules
        | consumeMatch: m =>
          // a perfectly normal EOF
          on(theTop).check
            *> Manip.pure(m)
        | consumeMatch: m =>
          addChild(Error("unexpected EOF", SourceMarker().at(m)))
            *> Manip.pure(m)
