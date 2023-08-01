package test.com.github.distcompiler.dcal

import utest.*
import chungus.Generator

import com.github.distcompiler.dcal.{DCalAST, Scoping}
import com.github.distcompiler.dcal.parsing.{SourceLocation, Ps}

object ScopingTests extends TestSuite {
  import DCalAST.*
  import Generator.*

  private given dummyLoc: SourceLocation = SourceLocation("dummy", offsetStart = -1, offsetEnd = -1)

  private given strGen: Generator[String] = chooseAny(List("foo", "bar", "ping"/*, "pong", "blamo"*/))
  private given numGen: Generator[BigInt] = chooseAny(List(BigInt(0), BigInt(10), BigInt(123456789)))

  private given psGen[T](using gen: Generator[T]): Generator[Ps[T]] = gen.map(Ps(_))

  private given limitedList[T](using gen: Generator[T]): Generator[List[T]] =
    listOf(gen, limit = 3)

  private given modGen: Generator[Module] =
    for {
      defns <- listOf(anyOf[Ps[Definition]], limit = 3)
    } yield Module(
      name = Ps("my_module"),
      imports = Nil,
      definitions = defns,
    )

  def tests = Tests {
    test("sanity") {
      anyOf[Module].forall(budget = 20) { module =>
        val (info, ()) = Scoping.scopeModule(module)(using Scoping.ScopingContext.empty).run.value
        if(info.referencePairs.nonEmpty) {
          println(info)
        }
      }
    }
  }  
}
