package distcompiler.tla

import cats.syntax.all.given

import distcompiler.*

object TLAReader extends Reader:
  lazy val wf = Wellformed:
    import distcompiler.wf.*
    Node.Top ::= repeated(tok(ModuleGroup))

    val allToks =
      tok(
        ModuleGroup,
        ParenthesesGroup,
        SqBracketsGroup,
        BracesGroup,
        TupleGroup,
        Alpha,
        LaTexLike,
        Comment
      )
        | Choice(NonAlpha.instances.toSet)
        | Choice(allOperators.toSet)

    ModuleGroup ::= allToks

    ParenthesesGroup ::= allToks
    SqBracketsGroup ::= allToks
    BracesGroup ::= allToks
    TupleGroup ::= allToks

    StringLiteral ::= Atom
    NumberLiteral ::= Atom
    Alpha ::= Atom
    LaTexLike ::= Atom

    Comment ::= Atom
    DashSeq ::= Atom

    allOperators.foreach(_ ::= Atom)
    NonAlpha.instances.foreach(_ ::= Atom)

  import dsl.*
  import Reader.*

  object StringLiteral extends Token.ShowSource
  object NumberLiteral extends Token.ShowSource

  object ModuleGroup extends Token
  object ParenthesesGroup extends Token
  object SqBracketsGroup extends Token
  object BracesGroup extends Token
  object TupleGroup extends Token

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
  case object `__` extends NonAlpha:
    override def spelling: String = "_"
  case object `!` extends NonAlpha
  case object `@` extends NonAlpha

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
      (!op.spelling.startsWith("\\") || op == Operators.`\\/` || op == Operators.`\\`)
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

    private def installAlphas(next: =>Manip[SourceRange]): bytes.selecting[SourceRange] =
      val nxt = defer(next)
      sel.onOneOf(letters):
        bytes.selectManyLike(letters ++ digits + '_'):
            consumeMatch: m =>
              addChild(Alpha(m))
                *> nxt

    private def installDotNumberLiterals: bytes.selecting[SourceRange] =
      digits.foldLeft(sel): (sel, d) =>
        sel.onSeq(s".$d")(numberLiteralAfterPoint)

  override protected lazy val rules: Manip[SourceRange] =
    moduleSearch.withTracer(Manip.LogTracer(os.write.over.outputStream(os.pwd / "tlaReader.log")))

  lazy val moduleSearchNeverMind: Manip[SourceRange] =
    on(
      tok(ModuleGroup) *> parent(theTop)
    ).value.flatMap: top =>
        dropMatch:
          effect(top.children.dropRightInPlace(1))
          *> atNode(top)(moduleSearch)

  private lazy val moduleSearch: Manip[SourceRange] =
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
                    *> moduleSearch)
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
        .installAlphas:
          // TODO: this should be one case.
          // Fields should pass idx along, and have .optional and .repeated elements
          val validCases =
            on(
              tok(ModuleGroup)
              *> children:
                Fields()
                  .skip(tok(DashSeq))
                  .skip(tok(Alpha).filterSrc("MODULE"))
                  .atEnd
            ).check
            | on(
              tok(ModuleGroup)
              *> children:
                Fields()
                  .skip(tok(DashSeq))
                  .skip(tok(Alpha).filterSrc("MODULE"))
                  .skip(tok(Alpha))
                  .atEnd
            ).check

          (validCases *> moduleSearch)
          | on(theTop).value.flatMap: top =>
            // Saw an alpha at top level. Delete it.
            effect(top.children.dropRightInPlace(1))
            *> moduleSearch
          | moduleSearchNeverMind // otherwise we failed to parse a module start
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
        .installAlphas(tokens)
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
    commit:
      bytes
        .selecting[SourceRange]
        .on('"'):
          consumeMatch: m =>
            addChild(StringLiteral(m))
              *> tokens
        .onSeq("\\\"")(stringLiteral)
        .fallback:
          bytes.selectOne:
            stringLiteral
          | unexpectedEOF

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
        .installDotNumberLiterals
        .fallback(endNumberLiteral)
