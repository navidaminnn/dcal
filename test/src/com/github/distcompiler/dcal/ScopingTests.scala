package test.com.github.distcompiler.dcal

import java.time.Duration
import cats.data.Chain

import utest.{TestSuite, test, Tests}
import chungus.{Generator, Checker, assert, assertMatch, recording, Counters}

import com.github.distcompiler.dcal.{DCalAST, Scoping}
import com.github.distcompiler.dcal.transform.Transform
import com.github.distcompiler.dcal.parsing.{SourceLocation, Ps, PsK}

import Scoping.ScopingError

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

  // limit string literals, because unlike other AST strings they don't matter
  private given strLiteralGen: Generator[Expression.StringLiteral] =
    one(Expression.StringLiteral("string"))

  private given numGen: Generator[BigInt] = one(BigInt(1)) // doesn't matter here

  private given psGen[T](using gen: Generator[T])(using DummyLocSrc): Generator[Ps[T]] = gen.map(Ps(_))

  private given limitedList[T](using gen: Generator[T]): Generator[List[T]] =
    listOf(gen, limit = 3)

  private given modGen(using DummyLocSrc)(using Generator[Ps[Definition]]): Generator[Module] =
    for {
      defns <- listOf(anyOf[Ps[Definition]], limit = 3)
    } yield Module(
      name = Ps("my_module"),
      imports = Nil,
      definitions = defns,
    )

  def findAllReferents(module: Module): Set[PsK[Scoping.Referent]] = {
    import Scoping.Referent

    given strEmpty: Transform[String, Chain[PsK[Referent]]] = _ => Chain.empty
    given bigIntEmpty: Transform[BigInt, Chain[PsK[Referent]]] = _ => Chain.empty

    given otherPs[T](using trans: Transform[T, Chain[PsK[Referent]]]): Transform[Ps[T], Chain[PsK[Referent]]] with {
      override def apply(from: Ps[T]): Chain[PsK[Referent]] = trans(from.value)
    }

    given findOpCall: Transform[Ps[Expression.OpCall], Chain[PsK[Referent]]] with {
      override def apply(from: Ps[Expression.OpCall]): Chain[PsK[Referent]] = Chain.one(from.toPsK.up)
    }

    given findStatementCall: Transform[Ps[Statement.Call], Chain[PsK[Referent]]] with {
      override def apply(from: Ps[Statement.Call]): Chain[PsK[Referent]] = Chain.one(from.toPsK.up)
    }

    given findBindingCall: Transform[Ps[Binding.Call], Chain[PsK[Referent]]] with {
      override def apply(from: Ps[Binding.Call]): Chain[PsK[Referent]] = Chain.one(from.toPsK.up)
    }

    Transform[Module, Chain[PsK[Referent]]](module)
      .iterator
      .toSet
  }

  // TODO: generate well-scoped modules and check them
  // TODO: and generate otherwise well-scoped modules with one scoping error inserted 

  def generalChecks(requiredReferences: Int, requiredErrors: Int)(moduleGen: Generator[Module]): Unit = {
    moduleGen.checkWith {
      import Checker.*
      timeLimited(maxDuration = Duration.ofMinutes(1), printRoundExample = false) {
        exists[Module](_.definitions.size >= 2)
        && transform[Module, (Module, Scoping.ScopingInfo)] { module =>
          val (info, ()) = Scoping.scopeModule(module)(using Scoping.ScopingContext.empty).run.value
          (module, info)
        } {
          exists[(Module, Scoping.ScopingInfo)](_._2.errors.size >= requiredErrors)
          && exists[(Module, Scoping.ScopingInfo)](_._2.referencePairs.size >= requiredReferences)
          && forall[(Module, Scoping.ScopingInfo)] {
            case (module, info) =>
              val errors = info.errors.toList
              val referencePairs = info.referencePairs.toList
              recording(errors) {
                recording(referencePairs) {
                  // all errors must be unique
                  recording(errors.toSet) {
                    assert(errors.size == errors.toSet.size)
                  }
                  // all referents must refer to exactly one thing
                  // e.g they must form a set of unique identifiers; no duplicates
                  val referents = referencePairs.map(_._1)
                  val referentSet = referents.toSet
                  recording(referents) {
                    recording(referentSet) {
                      assert(referents.size == referentSet.size)
                    }
                  }
                  // all references in general must be unique
                  recording(referencePairs.toSet) {
                    assert(referencePairs.size == referencePairs.toSet.size)
                  }
                  
                  referencePairs.foreach {
                    case (from, to) =>
                      recording((from, to)) {
                        assertMatch((from, to)) {
                          case (PsK(Expression.OpCall(_, arguments), _), PsK(Definition(_, params, _), _)) if arguments.size == params.size =>
                          case (PsK(Expression.OpCall(Right(Ps(Path.Name(name1))), Nil), _), PsK(Statement.Let(Ps(name2), _), _)) if name1 == name2 =>
                          case (PsK(Expression.OpCall(Right(Ps(Path.Name(name1))), Nil), _), PsK(Statement.Var(Ps(name2), _), _)) if name1 == name2 =>
                          //case (PsK(Expression.OpCall(Right(Ps(Path.Name(name1))), Nil), _), PsK(name2: String, _)) if name1 == name2 =>
                          case (PsK(Binding.Call(_, arguments), _), PsK(Definition(name, params, _), _)) if arguments.size == params.size =>
                          case (PsK(Statement.Call(Ps(Binding.Call(_, arguments))), _), PsK(Definition(_, params, _), _)) if arguments.size == params.size =>
                        }
                      }
                  }

                  recording(referentSet) {
                    errors.foreach {
                      case ScopingError.Redefinition(first, second) => 
                      case ScopingError.UndefinedReference(ref) =>
                        recording(ref) {
                          assert(!referentSet.contains(ref))
                        }
                      case ScopingError.ArityMismatch(ref, defn) =>
                        recording(ref) {
                          assert(!referentSet.contains(ref))
                        }
                      case ScopingError.KindMismatch(ref, defn) =>
                        recording(ref) {
                          assert(!referentSet.contains(ref))
                        }
                    }
                  }

                  // if no errors, all referents must be accounted for
                  if(errors.isEmpty) {
                    val allReferents = findAllReferents(module)
                    recording(allReferents) {
                      recording(referentSet) {
                        assert(allReferents == referentSet)
                      }
                    }
                  }
                }
              }
          }
        }
      }
    }
  }

  def tests = Tests {
    test("generalChecks") {
      // test("any") {
      //   generalChecks(minReferences = 2, minErrors = 2) {
      //     given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()
      //     given strGen: Generator[String] = chooseAny(List("foo", "bar", "ping"))
      //     anyOf[Module]
      //   }
      // }
      test("lots of names") {
        // simplify state space by avoiding generating too many extraneous exprs and stmts
        // this should be mostly names and defs
        generalChecks(requiredReferences = 3, requiredErrors = 3) {
          given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()

          // more names to choose from
          given strGen: Generator[String] = chooseAny(('a' to 'd').map(_.toString).toList)

          object ExprKey extends Counters.Key[Expression]
          object StmtKey extends Counters.Key[Statement]
          object BlockKey extends Counters.Key[Statement.Block]
          object BindingKey extends Counters.Key[Binding]

          // don't pay for defn block
          given defnGen: Generator[Definition] =
            for {
              name <- anyOf[Ps[String]]
              params <- listOf(anyOf[Ps[DefParam]], limit = 3)
              bodyStmts <- listOf(anyOf[Ps[Statement]], limit = 3)
            } yield Definition(name, params, Ps(Statement.Block(bodyStmts)))

          given bindingGen: Generator[Binding] =
            lzy {
              anyOf[Binding.Value]
              | rateLimit(key = BindingKey, limit = 0)(Generator.anySum[Binding])
            }

          given stmtGen: Generator[Statement] =
            lzy {
              Generator.anyProduct[Statement.Let]
              | Generator.anyProduct[Statement.Var]
              | rateLimit(key = StmtKey, limit = 0)(Generator.anyProduct[Statement.Block].up)
              | rateLimit(key = StmtKey, limit = 0)(Generator.anySum[Statement])
            }

          given exprGen: Generator[Expression] =
            lzy {
              anyOf[Expression.OpCall]
              | rateLimit(key = ExprKey, limit = 0)(Generator.anySum[Expression])
            }

          anyOf[Module]
        }
      }
    }
  }  
}
