package test.distcompiler.dcal

import cats.syntax.all.given

import distcompiler.transform.*
import distcompiler.dcal.*
import distcompiler.parsing.{Ps, SourceLocation}

class TokenizerTests extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(1, scala.concurrent.duration.HOURS)

  import Generator.*

  given Generatable[Token] = Generatable.derived
  given Generatable[Keyword] = Generatable.derived
  given Generatable[Punctuation] = Generatable.derived
  given Generatable[BinaryOperator] = Generatable.derived

  private given dummyLoc: SourceLocation = SourceLocation(path = "<dummy>", offsetStart = -1, offsetEnd = -1)
  
  lazy val tokenGen: Generator[Token] =
    locally {
      val anyNameChar: Generator[Char] =
        anyFromSeq("0_ifX") // conveniently include letters needed for the 'if' keyword
      val anyNameChars: Generator[String] =
        listOf(anyNameChar, limit = 3)
          .map(_.mkString)
      val anyStringChar: Generator[Char] =
        anyNameChar
        | anyFromSeq("\"\n\t\\")
      val anyStringChars: Generator[String] = listOf(anyStringChar, limit = 3).map(_.mkString)

      Generatable[Token]
        .build
        .replace[BigInt] {
          anyFromSeq(List(BigInt(0), BigInt(10), BigInt(123456789)))
        }
        .replace[Token.Name] {
          anyNameChars
            .filter(_.nonEmpty)
            .filter(_.exists(Character.isAlphabetic))
            .filter(name => !Keyword.stringSet(name))
            .map(Token.Name(_))
        }
        .replace[Token.StringLiteral] {
          anyStringChars.map(Token.StringLiteral(_))
        }
        .apply
    }

  enum Whitespace derives Generatable {
    case ` `
    case `\n`
    case `\t`
  }

  private def renderSeq(tokensOrSpace: List[Token | Whitespace]): String =
    tokensOrSpace
      .iterator
      .flatMap[Char] {
        case ws: Whitespace => ws.productPrefix
        case Token.IntLiteral(value) => value.toString()
        case Token.StringLiteral(value) =>
          Iterator.single('"')
          ++ value
            .iterator
            .flatMap {
              case '"' => "\\\""
              case '\n' => "\\n"
              case '\t' => "\\t"
              case '\\' => "\\\\"
              case ch => Iterator.single(ch)
            }
          ++ Iterator.single('"')
        case Token.Name(name) => name
        case Token.Keyword(kw) => kw.name
        case Token.Punctuation(pn) => pn.name
      }
      .mkString

  private def adjacentPairRules(tokensOrSpace: List[Token | Whitespace]): Boolean =
    if(tokensOrSpace.isEmpty) {
      true
    } else {
      (tokensOrSpace.iterator zip tokensOrSpace.tail).forall {
        // that's just one number at that point
        case (Token.IntLiteral(_), Token.IntLiteral(_)) => false
        // names will just "eat" ints, keywords, or token literals after them
        case (Token.Name(_), Token.Keyword(_) | Token.IntLiteral(_) | Token.Name(_)) => false
        // putting an int just before alphanumerics looks like a name
        case (Token.IntLiteral(_), Token.Keyword(_) | Token.Name(_)) => false
        // if two elements of punctuation concatenate to form also valid punctuation, they need a space separating them
        case (Token.Punctuation(l), Token.Punctuation(r)) if Punctuation.values.iterator.map(_.name).contains(l.name ++ r.name) => false
        case _ => true
      }
    }

  test("to string and back") {
    // if this is false some of our checks will be vacuous
    assert(Keyword.values.exists(_.name.size <= 2))

    listOf(tokenGen | Generatable[Whitespace].any, limit = 3)
      .filter(adjacentPairRules)
      .toChecker
      .exists(_.size >= 3)
      .exists(_.exists {
        case Token.Name(name) if name.size >= 3 => true
        case _ => false
      })
      .exists(_.exists {
        case Token.StringLiteral(value) if value.size >= 3 => true
        case _ => false
      })
      .forall { tokensOrSpace =>
        val strForm = renderSeq(tokensOrSpace)
        val reparsedTokens = Tokenizer(strForm, path = "<dummy>").toList
        val expectedtokens = tokensOrSpace
          .collect { case tok: Token => tok }
          .map(Ps(_))
          .map(Right(_))

        assertEquals(reparsedTokens, expectedtokens, clue = strForm)
      }
      .run()
  }
}
