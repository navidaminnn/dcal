package distcompiler.tla

import cats.syntax.all.given

import distcompiler.*

object TLAReader extends Reader:
  lazy val wellformed = Wellformed:
    import distcompiler.wf.*
    Node.Top ::= repeated(tok(ModuleGroup))

    val allToks =
      tok(
        StringLiteral,
        NumberLiteral,
        // ---
        ModuleGroup,
        ParenthesesGroup,
        SqBracketsGroup,
        BracesGroup,
        TupleGroup,
        LetGroup,
        // ---
        Alpha,
        LaTexLike,
        // ---
        Comment,
        DashSeq
      )
        | Choice(NonAlpha.instances.toSet)
        | Choice(allOperators.toSet)

    ModuleGroup ::= allToks

    ParenthesesGroup ::= allToks
    SqBracketsGroup ::= allToks
    BracesGroup ::= allToks
    TupleGroup ::= allToks
    LetGroup ::= allToks

    StringLiteral ::= Atom
    Alpha ::= Atom
    LaTexLike ::= Atom

    Comment ::= Atom
    DashSeq ::= Atom

    allOperators.foreach(_ ::= Atom)
    NonAlpha.instances.foreach(_ ::= Atom)

  import dsl.*
  import Reader.*

  // override protected def tracePathOpt: Option[os.Path] = Some(os.pwd / "tlareader.log")
  // override protected def traceLimit: Int = 200

  object StringLiteral extends Token.ShowSource
  object NumberLiteral extends Token.ShowSource

  object ModuleGroup extends Token
  object ParenthesesGroup extends Token
  object SqBracketsGroup extends Token
  object BracesGroup extends Token
  object TupleGroup extends Token
  object LetGroup extends Token

  object Alpha extends Token.ShowSource
  object LaTexLike extends Token.ShowSource

  object Comment extends Token.ShowSource
  object DashSeq extends Token.ShowSource

  sealed trait NonAlpha extends Product, Token:
    def spelling: String = productPrefix
  object NonAlpha extends util.HasInstanceArray[NonAlpha]

  case object `:` extends NonAlpha
  case object `|->` extends NonAlpha
  case object `,` extends NonAlpha
  case object `.` extends NonAlpha
  case object `!` extends NonAlpha
  case object `@` extends NonAlpha
  case object `==` extends NonAlpha

  case object `WF_` extends NonAlpha
  case object `SF_` extends NonAlpha

  val digits: Set[Char] =
    ('0' to '9').toSet
  val letters: Set[Char] =
    ('a' to 'z').toSet ++ ('A' to 'Z').toSet
  val spaces: Set[Char] =
    Set(' ', '\t', '\n', '\r')

  val allOperators =
    Operators.PrefixOperator.instances
      ++ Operators.InfixOperator.instances
      ++ Operators.PostfixOperator.instances

  val nonAlphaNonLaTexOperators: IArray[Operators.Operator] =
    allOperators.filter: op =>
      (!op.spelling.startsWith(
        "\\"
      ) || op == Operators.`\\/` || op == Operators.`\\`)
        && !letters(op.spelling.head)

  extension (sel: bytes.selecting[SourceRange])
    private def installNonAlphaNonLatexOperators: bytes.selecting[SourceRange] =
      nonAlphaNonLaTexOperators.foldLeft(sel): (sel, op) =>
        sel.onSeq(op.spelling):
          consumeMatch: m =>
            addChild(op(m))
              *> tokens

    private def installNonAlphas: bytes.selecting[SourceRange] =
      NonAlpha.instances.foldLeft(sel): (sel, nonAlpha) =>
        sel.onSeq(nonAlpha.spelling):
          consumeMatch: m =>
            addChild(nonAlpha(m))
              *> tokens

    private def installLatexLikes: bytes.selecting[SourceRange] =
      lazy val impl =
        bytes.selectManyLike(letters):
          consumeMatch: m =>
            addChild(LaTexLike(m))
              *> tokens
      letters.foldLeft(sel): (sel, l) =>
        sel.onSeq(s"\\$l")(impl)

    private def installAlphas(
        next: => Manip[SourceRange]
    ): bytes.selecting[SourceRange] =
      val nxt = defer(next)
      sel.onOneOf(letters + '_'):
        bytes.selectManyLike(letters ++ digits + '_'):
          consumeMatch: m =>
            addChild(Alpha(m))
              *> nxt

    private def installDotNumberLiterals: bytes.selecting[SourceRange] =
      digits.foldLeft(sel): (sel, d) =>
        sel.onSeq(s".$d")(numberLiteralAfterPoint)

  override protected lazy val rules: Manip[SourceRange] =
    moduleSearch

  lazy val moduleSearchNeverMind: Manip[SourceRange] =
    on(
      tok(ModuleGroup) *> parent(theTop)
    ).value.flatMap: top =>
      dropMatch:
        effect(top.children.dropRightInPlace(1))
          *> atNode(top)(moduleSearch)

  private lazy val moduleSearch: Manip[SourceRange] =
    lazy val onAlpha: Manip[SourceRange] =
      val validCases =
        on(
          tok(ModuleGroup)
          *> children:
            Fields()
              .skip(tok(DashSeq))
              .skip(tok(Alpha).filterSrc("MODULE"))
              .optionalSkip(tok(Alpha))
              .atEnd
        ).check

      (validCases *> moduleSearch)
      | on(theTop).value.flatMap: top =>
        // Saw an alpha at top level. Delete it.
        effect(top.children.dropRightInPlace(1))
          *> moduleSearch
      | moduleSearchNeverMind // otherwise we failed to parse a module start

    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(spaces)(dropMatch(moduleSearch))
        .onSeq("----"):
          bytes.selectManyLike(Set('-')):
            commit:
              (on(theTop).check
              *> addChild(ModuleGroup())
                .here:
                  consumeMatch: m =>
                    addChild(DashSeq(m))
                      *> moduleSearch
              )
                | (on(
                  tok(ModuleGroup)
                  *> children:
                    Fields()
                      .skip(tok(DashSeq))
                      .skip(tok(Alpha).filterSrc("MODULE"))
                      .skip(tok(Alpha))
                      .atEnd
                ).check *> consumeMatch: m =>
                  addChild(DashSeq(m))
                    *> tokens)
                | moduleSearchNeverMind
        .installAlphas(onAlpha)
        .onOneOf(digits):
          // This case handles enough of the number -> alpha promotion that we can parse
          // the module name if it starts with one or more digits.
          bytes.selectManyLike(digits):
            bytes
              .selecting[SourceRange]
              .installAlphas(onAlpha)
              .fallback:
                on(theTop).check
                // Saw digits (?) at top level. We didn't even make a node, so just drop the match and go back to
                // business as usual.
                  *> dropMatch(moduleSearch)
                  | moduleSearchNeverMind // Or we saw an integer in the middle of module pattern, in which case drop this match.
        .fallback:
          bytes.selectOne:
            commit:
              (on(theTop).check *> moduleSearch)
                | moduleSearchNeverMind
          | consumeMatch: m =>
            on(theTop).check *> Manip.pure(m)
              | moduleSearchNeverMind

  private def openGroup(tkn: Token): Manip[SourceRange] =
    consumeMatch: m =>
      addChild(tkn(m))
        .here(tokens)

  private def closeGroup(
      tkn: Token,
      reinterpretTok: Token
  ): Manip[SourceRange] =
    val rest: Manip[SourceRange] =
      if tkn == reinterpretTok
      then on(tok(tkn)).check *> extendThisNodeWithMatch(atParent(tokens))
      else
        on(tok(tkn)).value.flatMap: node =>
          effect(
            node.replaceThis(reinterpretTok(node.unparentedChildren).like(node))
          )
            *> tokens

    on(tok(tkn)).check *> rest

  private def closeGroup(tkn: Token): Manip[SourceRange] =
    closeGroup(tkn, tkn)

  private def invalidGroupClose(tkn: Token, name: String): Manip[SourceRange] =
    // TODO: close a parent group if you can
    consumeMatch: m =>
      addChild(
        Builtin.Error(
          s"unexpected end of $name group",
          Builtin.SourceMarker(m)
        )
      )
        *> tokens

  private lazy val unexpectedEOF: Manip[SourceRange] =
    consumeMatch: m =>
      addChild(
        Builtin.Error(
          "unexpected EOF",
          Builtin.SourceMarker(m)
        )
      )
        *> Manip.pure(m)

  private lazy val letGroupSemantics: Manip[SourceRange] =
    commit:
      on(
        lastChild(tok(Alpha).filterSrc("LET"))
      ).value.flatMap: node =>
        effect(node.replaceThis(LetGroup(node.sourceRange)))
          .here(tokens)
      | on(
        tok(LetGroup) *> lastChild(tok(Alpha).filterSrc("IN"))
      ).value.flatMap: node =>
        effect(node.removeThis().sourceRange)
          .flatMap: m =>
            extendThisNode(m)
            atParent(tokens)
      | on(
        lastChild(tok(Alpha).filterSrc("IN"))
      ).value.flatMap: node =>
        effect(node.removeThis().sourceRange)
          .flatMap: m =>
            addChild(
              Builtin.Error(
                s"unexpected end of LET group",
                Builtin.SourceMarker(m)
              )
            )
              *> tokens
      | on(not(lastChild(tok(Alpha).filterSrc("IN")))).check *> tokens

  private lazy val tokens: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(spaces)(dropMatch(tokens))
        .onSeq("===="):
          bytes.selectManyLike(Set('=')):
            on(tok(ModuleGroup)).check
              *> extendThisNodeWithMatch(atParent(moduleSearch))
              | consumeMatch: m =>
                addChild(
                  Builtin.Error(
                    "unexpected end of module",
                    Builtin.SourceMarker(m)
                  )
                )
                  *> on(ancestor(theTop)).value.here(moduleSearch)
        .onSeq("----"):
          bytes.selectManyLike(Set('-')):
            consumeMatch: m =>
              addChild(DashSeq(m))
                *> tokens
        .on('"')(stringLiteral)
        .onSeq("(*")(multiComment)
        .onSeq("\\*")(lineComment)
        .on('(')(openGroup(ParenthesesGroup))
        .on(')'):
          closeGroup(ParenthesesGroup)
            | invalidGroupClose(ParenthesesGroup, "parentheses")
        .on('[')(openGroup(SqBracketsGroup))
        .on(']'):
          closeGroup(SqBracketsGroup)
            | invalidGroupClose(SqBracketsGroup, "square brackets")
        .on('{')(openGroup(BracesGroup))
        .on('}'):
          closeGroup(BracesGroup)
            | invalidGroupClose(BracesGroup, "braces")
        .onSeq("<<")(openGroup(TupleGroup))
        .onSeq(">>"):
          closeGroup(TupleGroup)
            | invalidGroupClose(TupleGroup, "tuple")
        .onOneOf(digits)(numberLiteral)
        .installDotNumberLiterals
        .installAlphas(letGroupSemantics)
        .installLatexLikes
        .installNonAlphaNonLatexOperators
        .installNonAlphas
        .fallback:
          bytes.selectOne:
            consumeMatch: m =>
              addChild(
                Builtin.Error(
                  "invalid character",
                  Builtin.SourceMarker(m)
                )
              )
                *> tokens
          | unexpectedEOF

  private lazy val lineComment: Manip[SourceRange] =
    val endComment: Manip[SourceRange] =
      consumeMatch: m =>
        addChild(Comment(m))
          *> tokens

    commit:
      bytes
        .selecting[SourceRange]
        .on('\n')(endComment)
        .onSeq("\r\n")(endComment)
        .fallback:
          bytes.selectOne(lineComment)

  private lazy val multiComment: Manip[SourceRange] =
    multiCommentRec:
      consumeMatch: m =>
        addChild(Comment(m))
          *> tokens

  private def multiCommentRec(outer: Manip[SourceRange]): Manip[SourceRange] =
    lazy val impl: Manip[SourceRange] =
      commit:
        bytes
          .selecting[SourceRange]
          .onSeq("*)")(outer)
          .onSeq("(*")(multiCommentRec(impl))
          .fallback:
            bytes.selectOne:
              impl

    impl

  private lazy val stringLiteral: Manip[SourceRange] =
    object builderRef extends Manip.Ref[SourceRange.Builder]

    def addByte(b: Byte): Manip[SourceRange] =
      dropMatch:
        builderRef.get
          .tapEffect(_.addOne(b))
          *> stringLiteralLoop

    def finishStringLiteral(rest: Manip[SourceRange]): Manip[SourceRange] =
      dropMatch:
        builderRef.get.flatMap: builder =>
          addChild(StringLiteral(builder.result()))
            *> rest

    lazy val stringLiteralLoop: Manip[SourceRange] =
      commit:
        bytes
          .selecting[SourceRange]
          .on('"')(finishStringLiteral(tokens))
          .onSeq("\\\"")(addByte('"'))
          .onSeq("\\\\")(addByte('\\'))
          .onSeq("\\t")(addByte('\t'))
          .onSeq("\\n")(addByte('\n'))
          .onSeq("\\f")(addByte('\f'))
          .onSeq("\\r")(addByte('\r'))
          .fallback:
            bytes.selectOne:
              consumeMatch: m =>
                assert(m.length == 1)
                addByte(m.head)
            | finishStringLiteral(unexpectedEOF)

    builderRef.init(SourceRange.newBuilder)(dropMatch(stringLiteralLoop))

  lazy val endNumberLiteral: Manip[SourceRange] =
    consumeMatch: m =>
      addChild(NumberLiteral(m))
        *> tokens

  private lazy val numberLiteralAfterPoint: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(digits)(numberLiteralAfterPoint)
        .fallback(endNumberLiteral)

  private lazy val numberLiteral: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(digits)(numberLiteral)
        // If we find a letter, then we're not in a num literal.
        // Act like we were processing an Alpha all along.
        .installAlphas(letGroupSemantics)
        .installDotNumberLiterals
        .fallback(endNumberLiteral)
