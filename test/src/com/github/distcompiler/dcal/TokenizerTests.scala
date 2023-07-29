package test.com.github.distcompiler.dcal

import utest.*
import chungus.*

import com.github.distcompiler.dcal.DCalTokenizer
import com.github.distcompiler.dcal.parsing.SourceLocation
import utest.asserts.Show

object TokenizerTests extends TestSuite {
  import DCalTokenizer.*
  import Generator.*
  
  val tokenGen =
    locally {
      given dummyLoc: SourceLocation = SourceLocation(path = "<dummy>", offsetStart = -1, offsetEnd = -1)

      given anyInt: Generator[BigInt] = one(BigInt(0)) | one(BigInt(10)) | one(BigInt(123456789)) 
      given anyString: Generator[String] = one("")
      given anyToken: Generator[Token] =
        anyOf[Keyword].map(Token.Keyword(_))
        | anyOf[Punctuation].map(Token.Punctuation(_))
        | anyOf[Operator].map(Token.Operator(_))
        | anyInt.map(Token.IntLiteral(_))
        | anyString.map(Token.Name(_))
        | anyString.map(Token.StringLiteral(_))

      anyToken
    }

  enum Whitespace {
    case ` `
    case `\n`
    case `\t`
  }

  def tests = Tests {
    test("to string and back") {
      def renderSeq(tokensOrSpace: List[Token | Whitespace]): String =
        tokensOrSpace.iterator
          .flatMap[Char] {
            case ws: Whitespace => ws.productPrefix
            case Token.IntLiteral(value) => value.toString()
            case Token.StringLiteral(value) => value
            case Token.Name(name) => name
            case Token.Operator(op) => op.name
            case Token.Keyword(kw) => kw.name
            case Token.Punctuation(pn) => pn.name
          }
          .mkString

      listOf(tokenGen | anyOf[Whitespace]).forall(budget = 3) { tokensOrSpace =>
        println(tokensOrSpace)

        @Show
        val strForm = renderSeq(tokensOrSpace)
        @Show
        val reparsedTokens = DCalTokenizer(strForm, path = "<dummy>").toList

        assert(reparsedTokens == tokensOrSpace.map(Right(_)))
      }
    }
  }
}
