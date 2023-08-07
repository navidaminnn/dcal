package test.com.github.distcompiler.dcal

import java.time.Duration
import cats.data.Chain
import cats.Applicative
import cats.syntax.all.given

import utest.{TestSuite, test, Tests}
import chungus.{Generator, Checker, assert, assertMatch, recording, Counters}

import com.github.distcompiler.dcal.{AST, Scoping}
import com.github.distcompiler.dcal.transform.Transform
import com.github.distcompiler.dcal.parsing.{SourceLocation, Ps, PsK}

import Scoping.ScopingError

object ScopingTests extends TestSuite {
  import AST.*
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
    pure(Expression.StringLiteral("string"))

  private given numGen: Generator[BigInt] = pure(BigInt(1)) // doesn't matter here

  private given psGen[T](using gen: Generator[T])(using DummyLocSrc): Generator[Ps[T]] = gen.map(Ps(_))

  private given limitedList[T](using gen: Generator[T]): Generator[List[T]] =
    listOf(gen, limit = 3)

  private given modGen(using DummyLocSrc)(using Generator[List[Ps[Definition]]]): Generator[Module] =
    anyOf[List[Ps[Definition]]].map { defns =>
      Module(
        name = Ps("my_module"),
        imports = Nil,
        definitions = defns,
      )
    }

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

  def generalChecks(requiredReferences: Int, requiredErrors: Int, requiredDefinitions: Int)(moduleGen: Generator[Module]): Unit = {
    moduleGen
      .toChecker
      .withPrintExamples(printExamples = false)
      .exists(_.definitions.size >= requiredDefinitions)
      .transform { module =>
        val (info, ()) = Scoping.scopeModule(module)(using Scoping.ScopingContext.empty).run.value
        (module, info)
      } { checker =>
        checker
          .exists(_._2.errors.size >= requiredErrors)
          .exists(_._2.referencePairs.size >= requiredReferences)
          .forall {
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
      .run()
  }
  
  // simplify state space by avoiding generating too many extraneous exprs and stmts
  // this should be mostly names and defs
  final class NameTargetedGens(definitionCount: Int)(using DummyLocSrc) {
    // names to choose from
    given strGen: Generator[String] = anyFromSeq(('a' to 'c').map(_.toString))

    object ExprKey extends Counters.Key[Expression]
    object StmtKey extends Counters.Key[Statement]
    object BindingKey extends Counters.Key[Binding]

    given definitionsGen: Generator[List[Ps[Definition]]] =
      listOf(anyOf[Ps[Definition]], limit = definitionCount)

    // don't pay for defn block
    given defnGen: Generator[Definition] =
      Applicative[Generator].product(
        anyOf[Ps[String]],
        Applicative[Generator].product(
          listOf(anyOf[Ps[DefParam]], limit = 3),
          listOf(anyOf[Ps[Statement]], limit = 3),
        )
      ).map {
        case (name, (params, bodyStmts)) =>
          Definition(name, params, Ps(Statement.Block(bodyStmts)))
      }

    given bindingGen: Generator[Binding] =
      lzy {
        anyOf[Binding.Value]
        //| rateLimit(key = BindingKey, limit = 1)(Generator.anySum[Binding])
      }
      .up

    given stmtGen: Generator[Statement] =
      lzy {
        Generator.anyProduct[Statement.Let]
        | Generator.anyProduct[Statement.Var]
        | Generator.anyProduct[Statement.Call]
        //| rateLimit(key = StmtKey, limit = 1)(Generator.anySum[Statement])
      }
      .up

    given exprGen: Generator[Expression] =
      lzy {
        pure(Expression.StringLiteral("<value>"))
        | anyOf[Expression.OpCall]
        //| rateLimit(key = ExprKey, limit = 1)(Generator.anySum[Expression])
      }
      .up
  }

  final case class ValidNames(names: List[String]) {
    def add(name: String): ValidNames =
      if(names.contains(name)) {
        this
      } else {
        copy(names = name :: names)
      }
  }

  object ValidNames {
    def empty: ValidNames = ValidNames(names = Nil)
  }

  class CorrectlyScopedGens(using DummyLocSrc) {
    val allNames = List("foo", "bar", "ping")

    given strGen: Generator[String] = anyFromSeq(allNames)

    given correctlyScopedOpCall(using validNames: ValidNames): Generator[Expression.OpCall] =
      (anyFromSeq(validNames.names)/*, anyOf[List[Ps[Expression]]]*/).map { (name/*, args*/) =>
        Expression.OpCall(Right(Ps(Path.Name(name))), Nil)
      }
    
    given correctlyScopedStmts(using validNames: ValidNames): Generator[List[Ps[Statement]]] =
      lzy {
        locally[Generator[List[Ps[Statement]]]] {
          allNames.foldMap { name =>
            val letGen = anyOf[Ps[Binding]].map(binding => Ps(Statement.Let(Ps(name), binding)))
            locally {
              given ValidNames = validNames.add(name)
              consOf(letGen, listOf(anyOf[Ps[Statement]], limit = 2))
            }
          }
        }
        | listOf(
            anySumOfShape[Statement]
              .excluding[Statement.Let]
              .excluding[Statement.Var]
              .summon
              .map(Ps(_)),
            limit = 2,
          )
      }
  }

  def tests = Tests {
    // test("generalChecks") {
    //   test("untargeted") {
    //     generalChecks(requiredReferences = 1, requiredErrors = 1, requiredDefinitions = 1) {
    //       given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()
    //       // very short lists or we're going to take forever
    //       given limitedList[T](using gen: Generator[T]): Generator[List[T]] =
    //         listOf(gen, limit = 2)
    //       given strGen: Generator[String] = anyFromSeq(List("foo", "bar", "ping"))
    //       anyOf[Module]
    //     }
    //   }
    //   test("1 definition") {
    //     generalChecks(requiredReferences = 3, requiredErrors = 3, requiredDefinitions = 1) {
    //       given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()

    //       val gens = new NameTargetedGens(definitionCount = 1)
    //       import gens.given
    //       anyOf[Module]
    //     }
    //   }
    //   test("3 definitions") {
    //     generalChecks(requiredReferences = 2, requiredErrors = 2, requiredDefinitions = 3) {
    //       given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()

    //       val gens = new NameTargetedGens(definitionCount = 3)
    //       import gens.given
    //       anyOf[Module]
    //     }
    //   }
    // }
    test("correct scoping") {
      given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()
      val gens = new CorrectlyScopedGens
      given ValidNames = ValidNames.empty

      import gens.given
      anyOf[Module]
        .toChecker
        .withPrintExamples(printExamples = false)
        .transform { module =>
          val (info, ()) = Scoping.scopeModule(module)(using Scoping.ScopingContext.empty).run.value
          (module, info)
        } { checker =>
          checker
            .exists(_._2.referencePairs.size >= 2)
            .forall {
              case (module, info) =>
                val referencePairs = info.referencePairs.toList
                val errors = info.errors.toList
                recording(referencePairs) {
                  recording(errors) {
                    assert(errors.isEmpty)
                  }
                }
            }
        }
        .run()
    }
  }  
}
