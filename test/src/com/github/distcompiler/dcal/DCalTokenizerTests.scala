package test.com.github.distcompiler.dcal

import utest.{TestSuite, Tests, test}
import chungus.*

import com.github.distcompiler.dcal.DCalTokenizer
import com.github.distcompiler.dcal.parsing.{Ps, SourceLocation}
import scala.concurrent.duration.FiniteDuration

object DCalTokenizerTests extends TestSuite {
  import DCalTokenizer.*
  import Generator.*

  private given dummyLoc: SourceLocation = SourceLocation(path = "<dummy>", offsetStart = -1, offsetEnd = -1)
  
  val tokenGen =
    locally {
      given anyInt: Generator[BigInt] = one(BigInt(0)) | one(BigInt(10)) | one(BigInt(123456789)) 
      val anyNameChar: Generator[Char] = (chooseAny('0' to '9') | chooseAny("_abcXYZ"))
      val anyNameChars: Generator[String] = listOf(anyNameChar).map(_.mkString)
      val anyStringChar: Generator[Char] = anyNameChar | chooseAny("\"\n\t\\")
      val anyStringChars: Generator[String] = listOf(anyStringChar).map(_.mkString)

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
    listOf(tokenGen | anyOf[Whitespace])
      .filter(adjacentPairRules)
      .checkWith {
        import Checker.*
        import java.time.Duration

        timeLimited(maxDuration = Duration.ofMinutes(1)) {
          exists[List[Token | Whitespace]](_.size >= 5)
          && forall { tokensOrSpace =>
            val strForm = renderSeq(tokensOrSpace)
            try {
              val reparsedTokens = DCalTokenizer(strForm, path = "<dummy>").toList
              val expectedtokens = tokensOrSpace
                .collect { case tok: Token => tok }
                .map(Ps(_))
                .map(Right(_))

              assert(reparsedTokens == expectedtokens)
            } catch {
              case err =>
                pprint(s"$tokensOrSpace ==> |$strForm")
                throw err
            }
          }
        }
      }
  }

  def tests = Tests {
    test("to string and back") - toStringAndBack()
  }
}
