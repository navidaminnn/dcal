package test.com.github.distcompiler.dcal

import java.time.Duration

import utest.*
import chungus.{Generator, Checker}

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
    // idea: all idents must refer to _something_, and _one_ thing
    // idea: all references have the right arity
    // idea: no recorded references may also be reference errors
    // idea: (more difficult) all well-scoped progs (need to generate) must be well-scoped

    // more generally, some reference errors must exist, and some references must exist (TODO: implement existence assertions, and maybe counting checks)
    test("sanity") {
      anyOf[Module].checkWith {
        import Checker.*
        timeLimited(maxDuration = Duration.ofMinutes(1)) {
          forall { module =>
            val (info, ()) = Scoping.scopeModule(module)(using Scoping.ScopingContext.empty).run.value
            if(info.referencePairs.nonEmpty) {
              println(info)
            }
          }
        }
      }
    }
  }  
}
