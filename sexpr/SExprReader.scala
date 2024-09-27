package distcompiler.sexpr

import cats.syntax.all.given

import distcompiler.*
import distcompiler.Reader.bytes.getSrc

object SExprReader extends Reader:
  import distcompiler.dsl.*
  import distcompiler.Builtin.{Error, SourceMarker}
  import Reader.*

  private val alpha: Set[Char] =
    ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  private val pseudoAlpha: Set[Char] =
    Set('-', '.', '/', '_', ':', '*', '+', '=')
  private val digit: Set[Char] = ('0' to '9').toSet
  private val whitespace: Set[Char] = Set(' ', '\n', '\r', '\t')

  protected val rules: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(whitespace):
          extendThisNodeWithMatch(rules)
        .on('('):
          addChild(tokens.List())
            .here(extendThisNodeWithMatch(rules))
        .on(')'):
          extendThisNodeWithMatch:
            atParent:
              rules
          | consumeMatch: m =>
            addChild(Error("unexpected end of list", SourceMarker().at(m)))
              *> rules
        .onOneOf(digit):
          rawMode
        .onOneOf(alpha ++ pseudoAlpha):
          tokenMode
        .on('"'):
          dropMatch(stringMode) // opening `"` is not part of string
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

  private val rawMode: Manip[SourceRange] =
    commit:
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

  private val tokenMode: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(alpha)(tokenMode)
        .onOneOf(pseudoAlpha)(tokenMode)
        .onOneOf(digit)(tokenMode)
        .fallback:
          consumeMatch: m =>
            addChild(tokens.Atom().at(m))
              *> rules

  private val stringMode: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .on('"'):
          consumeMatch: m =>
            val stringContents = m.dropRight(1)
            addChild(tokens.String().at(stringContents))
              *> rules
        .onSeq("\\\\")(stringMode)
        .onSeq("\\\"")(stringMode)
        .onSeq("\\'")(stringMode)
        .onSeq("\\n")(stringMode)
        .onSeq("\\t")(stringMode)
        .onSeq("\\r")(stringMode)
        .onSeq("\\\n")(stringMode)
        .onSeq("\\\r")(stringMode)
        .onSeq("\\\r\n")(stringMode)
        .onSeq("\\\n\r")(stringMode)
        .on('\\'):
          getSrc.flatMap: src =>
            addChild(
              Error(
                "invalid string escape",
                SourceMarker().at(src.emptyAtOffset.extendLeft)
              )
            )
          *> stringMode
        .fallback:
          bytes.selectOne:
            stringMode
          | rules
