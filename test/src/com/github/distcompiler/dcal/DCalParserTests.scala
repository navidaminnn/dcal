package test.com.github.distcompiler.dcal

import utest.*
import chungus.Generator
import com.github.distcompiler.dcal.DCalAST
import com.github.distcompiler.dcal.parsing.SourceLocation

object DCalParserTests extends TestSuite {
  import Generator.*

  private given dummyLoc: SourceLocation = SourceLocation("dummy", offsetStart = -1, offsetEnd = -1)

  val nameGen: Generator[String] = chooseAny(List("foo", "bar", "ping", "pong", "blamo"))
  val numGen: Generator[BigInt] = chooseAny(List(BigInt(0), BigInt(10), BigInt(123456789), BigInt(10)))

  lazy val statementGen: Generator[DCalAST.Statement] = ???//anyOf

  // val definitionGen: Generator[DCalAST.Definition] =
  //   for {
  //     name <- nameGen
  //     params <- listOf(nameGen)
  //     body <- listOf(statementGen)
  //   } yield DCalAST.Definition(name, params, DCalAST.Statement.Block(body))

  // val moduleGen: Generator[DCalAST.Module] =
  //   for {
  //     name <- nameGen
  //     imports <- listOf(nameGen)
  //     definitions <- listOf(definitionGen)
  //   } yield DCalAST.Module(name, imports, definitions)
  
  def tests = Tests {
    test("to tokens and back") {
      ???
    }
  }
}
