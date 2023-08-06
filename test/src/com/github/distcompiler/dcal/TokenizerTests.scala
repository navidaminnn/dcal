package test.com.github.distcompiler.dcal

import cats.syntax.all.given

import utest.{TestSuite, Tests, test}
import chungus.*

import com.github.distcompiler.dcal.{Tokenizer, Token, Keyword}
import com.github.distcompiler.dcal.parsing.{Ps, SourceLocation}

object TokenizerTests extends TestSuite {
  import Tokenizer.*
  import Generator.*

  private given dummyLoc: SourceLocation = SourceLocation(path = "<dummy>", offsetStart = -1, offsetEnd = -1)
  
  val tokenGen =
    locally {
      given anyInt: Generator[BigInt] = pure(BigInt(0)) | pure(BigInt(10)) | pure(BigInt(123456789))
      val anyNameChar: Generator[Char] =
        anyFromSeq(List('0', '9'))
        | anyFromSeq("_aifX") // conveniently include letters needed for the 'if' keyword
      val anyNameChars: Generator[String] =
        listOf(anyNameChar, limit = 3)
          .map(_.mkString)
          .filter(name => !Keyword.values.iterator.map(_.name).exists(name.contains))
      val anyStringChar: Generator[Char] =
        anyNameChar
        | anyFromSeq("\"\n\t\\")
      val anyStringChars: Generator[String] = listOf(anyStringChar, limit = 3).map(_.mkString)

      given Generator[Token.Name] =
        anyNameChars
          .filter(_.nonEmpty)
          .filter(_.exists(Character.isAlphabetic))
          .map(Token.Name(_))

      given Generator[Token.StringLiteral] =
        anyStringChars.map(Token.StringLiteral(_))

      anyOf[Token]
    }

  enum Whitespace {
    case ` `
    case `\n`
    case `\t`
  }

  private def renderSeq(tokensOrSpace: List[Token | Whitespace]): String =
    tokensOrSpace.iterator
      .flatMap[Char] {
        case ws: Whitespace => ws.productPrefix
        case Token.IntLiteral(value) => value.toString()
        case Token.StringLiteral(value) =>
          Iterator.single('"')
          ++ value.iterator
            .flatMap {
              case '"' => "\\\""
              case '\n' => "\\n"
              case '\t' => "\\t"
              case '\\' => "\\\\"
              case ch => Iterator.single(ch)
            }
          ++ Iterator.single('\"')
        case Token.Name(name) => name
        case Token.BinaryOperator(op) => op.name
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
        case _ => true
      }
    }

  def toStringAndBack(): Unit = {
    // if this is false some of our checks will be vacuous
    assert(Keyword.values.exists(_.name.size <= 2))

    listOf(tokenGen | anyOf[Whitespace], limit = 2)
      .filter(adjacentPairRules)
      .toChecker
      .exists(_.size >= 2)
      .exists(_.exists {
        case Token.Name(name) if Keyword.stringSet(name) => true
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

        recording(strForm) {
          assert(reparsedTokens == expectedtokens)
        }
      }
      .run()
  }

  def tests = Tests {
    test("to string and back") - toStringAndBack()
  }
}
