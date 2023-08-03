package test.com.github.distcompiler.dcal

import java.time.Duration

import utest.*
import chungus.{Generator, Checker}

import com.github.distcompiler.dcal.{DCalAST, Scoping}
import com.github.distcompiler.dcal.parsing.{SourceLocation, Ps, PsK}

object ScopingTests extends TestSuite {
  import DCalAST.*
  import Generator.*

  trait DummyLocSrc {
    def getDummyLoc(): SourceLocation
  }

  final class IncreasingDummyLocSrc extends DummyLocSrc {
    private var pos = 0
    def getDummyLoc(): SourceLocation = {
      pos += 1
      SourceLocation("<incdummy>", offsetStart = pos, offsetEnd = pos)
    }
  }

  private given dummyLoc(using src: DummyLocSrc): SourceLocation =
    src.getDummyLoc()

  private given strGen: Generator[String] = chooseAny(List("foo", "bar", "ping"/*, "pong", "blamo"*/))
  private given numGen: Generator[BigInt] = chooseAny(List(BigInt(0), BigInt(10), BigInt(123456789)))

  private given psGen[T](using gen: Generator[T])(using DummyLocSrc): Generator[Ps[T]] = gen.map(Ps(_))

  private given limitedList[T](using gen: Generator[T]): Generator[List[T]] =
    listOf(gen, limit = 3)

  private given modGen(using DummyLocSrc): Generator[Module] =
    for {
      defns <- listOf(anyOf[Ps[Definition]], limit = 3)
    } yield Module(
      name = Ps("my_module"),
      imports = Nil,
      definitions = defns,
    )

  def generalChecks(): Unit = {
    given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()
    // idea: all idents must refer to _something_, and _one_ thing
    // idea: all references have the right arity
    // idea: no recorded references may also be reference errors
    // idea: (more difficult) all well-scoped progs (need to generate) must be well-scoped

    // more generally, some reference errors must exist, and some references must exist (TODO: implement existence assertions, and maybe counting checks)
    anyOf[Module].checkWith {
      import Checker.*
      timeLimited(maxDuration = Duration.ofMinutes(1), printRoundExample = false) {
        transform[Module, Scoping.ScopingInfo] { module =>
          val (info, ()) = Scoping.scopeModule(module)(using Scoping.ScopingContext.empty).run.value
          info
        } {
          exists[Scoping.ScopingInfo](_.errors.size >= 3)
          && exists[Scoping.ScopingInfo](_.referencePairs.size >= 3)
          && forall[Scoping.ScopingInfo] { (info: Scoping.ScopingInfo) =>
            val errors = info.errors.toList
            val referencePairs = info.referencePairs.toList

            // all errors must be unique
            assert(errors.size == errors.toSet.size)
            // all referents must refer to exactly one thing
            val referents = referencePairs.map(_._1)
            assert(referents.size == referents.toSet.size)
            // all references in general must be unique
            assert(referencePairs.size == referencePairs.toSet.size)
            
            referencePairs.foreach {
              case (from, to) =>
                assertMatch((from, to)) {
                  case (PsK(Expression.OpCall(_, arguments), _), PsK(Definition(_, params, _), _)) if arguments.size == params.size =>
                  case (PsK(Expression.OpCall(_, Nil), _), PsK(Statement.Let(_, _), _)) =>
                  case (PsK(Expression.OpCall(_, Nil), _), PsK(Statement.Var(_, _), _)) =>
                  case (PsK(Expression.OpCall(_, Nil), _), PsK(_: String, _)) =>
                  case (PsK(Binding.Call(_, arguments), _), PsK(Definition(_, params, _), _)) if arguments.size == params.size =>
                  case (PsK(Statement.Call(Ps(Binding.Call(_, arguments))), _), PsK(Definition(_, params, _), _)) if arguments.size == params.size =>
                }
            }
          }
        }
      }
    }
  }

  def tests = Tests {
    test("generalChecks") - generalChecks()
  }  
}
