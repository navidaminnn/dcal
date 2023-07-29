package test.com.github.distcompiler.dcal

import utest.*
import chungus.*

import com.github.distcompiler.dcal.DCalTokenizer
import com.github.distcompiler.dcal.parsing.SourceLocation

object DCalTokenizerTests extends TestSuite {
  import DCalTokenizer.*
  import Generator.*
  
  val tokenGen =
    locally {
      given dummyLoc: SourceLocation = SourceLocation(path = "<dummy>", offsetStart = -1, offsetEnd = -1)

      val anyInt: Generator[BigInt] = one(BigInt(0)) | one(BigInt(10)) | one(BigInt(123456789)) 
      val anyNameChar: Generator[Char] = (chooseAny('0' to '9') | chooseAny("_abcXYZ"))
      val anyNameChars: Generator[String] = listOf(anyNameChar).map(_.mkString)
      val anyStringChar: Generator[Char] = anyNameChar | chooseAny("\"\n\t\\")
      val anyStringChars: Generator[String] = listOf(anyStringChar).map(_.mkString)
      val anyToken: Generator[Token] =
        anyOf[Keyword].map(Token.Keyword(_))
        | anyOf[Punctuation].map(Token.Punctuation(_))
        | anyOf[Operator].map(Token.Operator(_))
        | anyInt.map(Token.IntLiteral(_))
        | anyNameChars.filter(_.nonEmpty).filter(_.exists(Character.isAlphabetic)).map(Token.Name(_))
        | anyStringChars.map(Token.StringLiteral(_))

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
            case Token.StringLiteral(value) => s"\"${
              value.iterator
                .flatMap {
                  case '"' => "\\\""
                  case '\n' => "\\n"
                  case '\t' => "\\t"
                  case '\\' => "\\\\"
                  case ch => Some(ch)
                }
                .mkString
            }\""
            case Token.Name(name) => name
            case Token.Operator(op) => op.name
            case Token.Keyword(kw) => kw.name
            case Token.Punctuation(pn) => pn.name
          }
          .mkString

      def adjacentPairRules(tokensOrSpace: List[Token | Whitespace]): Boolean =
        if(tokensOrSpace.isEmpty) {
          true
        } else {
          (tokensOrSpace.iterator zip tokensOrSpace.tail).forall {
            // that's just one number at that point
            case (Token.IntLiteral(_), Token.IntLiteral(_)) => false
            // names will just "eat" anything after them
            case (Token.Name(_), Token.Keyword(_) | Token.IntLiteral(_) | Token.Name(_)) => false
            // putting an int just before alphanumerics looks like a name
            case (Token.IntLiteral(_), Token.Keyword(_) | Token.Name(_)) => false
            case _ => true
          }
        }

      listOf(tokenGen | anyOf[Whitespace])
        .filter(adjacentPairRules)
        .forall(budget = 9) { tokensOrSpace =>
          val strForm = renderSeq(tokensOrSpace)
          try {
            val reparsedTokens = DCalTokenizer(strForm, path = "<dummy>").toList
            val expectedtokens = tokensOrSpace
              .collect { case tok: Token => tok }
              .map(Right(_))

            
            assert(reparsedTokens == expectedtokens)
          } catch {
            case err =>
              println(s"$tokensOrSpace ==> |$strForm")
              throw err
          }
        }
    }
  }
}
