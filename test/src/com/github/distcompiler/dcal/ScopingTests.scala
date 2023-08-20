package test.com.github.distcompiler.dcal

import scala.collection.immutable.ListMap

import cats.data.Chain
import cats.*
import cats.syntax.all.given

import chungus.*

import com.github.distcompiler.dcal.{AST, Scoping}
import com.github.distcompiler.dcal.parsing.{SourceLocation, Ps, PsK}
import com.github.distcompiler.dcal.transform.Transform
import com.github.distcompiler.dcal.transform.instances.all.given

import Scoping.ScopingError

class ScopingTests extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(1, scala.concurrent.duration.HOURS)

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

    given strEmpty: Transform[String, Chain[PsK[Referent]]] = Transform.fromFunction(_ => Chain.empty)
    given bigIntEmpty: Transform[BigInt, Chain[PsK[Referent]]] = Transform.fromFunction(_ => Chain.empty)

    given findInExpression: Transform.Refined[Ps[Expression], Chain[PsK[Referent]]] =
      Transform.Refined(rec => {
        case from @ Ps(opCall: Expression.OpCall) =>
          Chain.one(from.as(opCall).toPsK.widen)
          ++ rec(from)
        case from => rec(from)
      })

    given findInStatement: Transform.Refined[Ps[Statement], Chain[PsK[Referent]]] =
      Transform.Refined(rec => {
        case from @ Ps(call @ Statement.Call(Ps(Binding.Call(path, arguments)))) =>
          Chain.one(from.as(call).toPsK.widen)
          ++ summon[Transform[Ps[Path], Chain[PsK[Referent]]]](path)
          ++ arguments.foldMap(findInExpression.asFunction)
        case from => rec(from)
      })

    given findInBinding: Transform.Refined[Ps[Binding], Chain[PsK[Referent]]] =
      Transform.Refined(rec => {
        case from @ Ps(call: Binding.Call) =>
          Chain.one(from.as(call).toPsK.widen)
          ++ rec(from)
        case from => rec(from)
      })

    summon[Transform[Module, Chain[PsK[Referent]]]](module)
      .iterator
      .toSet
  }

  // TODO: generate otherwise well-scoped modules with one scoping error inserted 

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

              assert(clue(errors).size == clue(errors.toSet).size, "all errors must be unique")

              // all referents must refer to exactly one thing
              // e.g they must form a set of unique identifiers; no duplicates
              val referents = referencePairs.map(_._1)
              val referentSet = referents.toSet
              assert(clue(referents).size == clue(referentSet).size, "all referents must refer to exactly one thing")

              // all references in general must be unique
              assert(clue(referencePairs).size == clue(referencePairs.toSet).size, "all references must be unique")
                 
              referencePairs.foreach {
                case (PsK(Expression.OpCall(_, arguments), _), PsK(Definition(_, params, _), _)) if arguments.size == params.size =>
                case (PsK(Expression.OpCall(Right(Ps(Path.Name(name1))), Nil), _), PsK(Statement.Let(Ps(name2), _), _)) if name1 == name2 =>
                case (PsK(Expression.OpCall(Right(Ps(Path.Name(name1))), Nil), _), PsK(Statement.Var(Ps(name2), _), _)) if name1 == name2 =>
                //case (PsK(Expression.OpCall(Right(Ps(Path.Name(name1))), Nil), _), PsK(name2: String, _)) if name1 == name2 =>
                case (PsK(Binding.Call(_, arguments), _), PsK(Definition(name, params, _), _)) if arguments.size == params.size =>
                case (PsK(Statement.Call(Ps(Binding.Call(_, arguments))), _), PsK(Definition(_, params, _), _)) if arguments.size == params.size =>
                case (from, to) =>
                  fail("reference pair forms pattern I don't recognize", clues(from, to))
              }

              errors.foreach {
                case ScopingError.Redefinition(first, second) => 
                case ScopingError.UndefinedReference(ref) =>
                  assert(!clue(referentSet).contains(clue(ref)))
                case ScopingError.ArityMismatch(ref, defn) =>
                  assert(!clue(referentSet).contains(clue(ref)))
                case ScopingError.KindMismatch(ref, defn) =>
                  assert(!clue(referentSet).contains(clue(ref)))
              }

              // if no errors, all referents must be accounted for
              if(errors.isEmpty) {
                val allReferents = findAllReferents(module)
                assertEquals(referentSet, allReferents)
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
      }
      .widen

    given stmtGen: Generator[Statement] =
      lzy {
        Generator.anyProduct[Statement.Let]
        | Generator.anyProduct[Statement.Var]
        | Generator.anyProduct[Statement.Call]
      }
      .widen

    given exprGen: Generator[Expression] =
      lzy {
        pure(Expression.StringLiteral("<value>"))
        | anyOf[Expression.OpCall]
      }
      .widen
  }

  final case class ValidNames(info: ListMap[String, ValidNames.Info]) {
    import ValidNames.*

    def add(name: String, info: ValidNames.Info): ValidNames =
      copy(info = this.info.updated(name, info))

    def arity(name: String): Int =
      info(name) match {
        case Val => 0
        case Def(arity) => arity
      }

    def defNames: List[String] =
      info
        .iterator
        .collect { 
          case (name, Def(_)) => name
        }
        .toList

    def valNames: List[String] =
      info
        .iterator
        .collect {
          case (name, Val) => name
        }
        .toList
  }

  object ValidNames {
    enum Info derives CanEqual {
      case Def(arity: Int)
      case Val
    }
    export Info.*

    def empty: ValidNames = ValidNames(info = ListMap.empty)
  }

  class CorrectlyScopedGens(maxDefns: Int)(using DummyLocSrc) {
    val allNames = List("foo", "bar", "ping")

    def definableName(using validNames: ValidNames): Generator[String] =
      anyFromSeq(allNames)

    def listOfDefinableNames(limit: Int, arityLimit: Int)(using validNames: ValidNames): Generator[List[(String, Int)]] =
      listOf(definableName.product(levelIdx(limit = arityLimit)), limit = limit)
        .filterDistinctBy(_._1)

    given strGen: Generator[String] = anyFromSeq(allNames)

    given correctlyScopedOpCall(using validNames: ValidNames): Generator[Expression.OpCall] =
      anyFromSeq(validNames.valNames).map { name =>
        Expression.OpCall(Right(Ps(Path.Name(name))), Nil)
      }
      // TODO: calls with non-0 arity

    given correctlyScopedDefns(using validNames: ValidNames): Generator[List[Ps[Definition]]] = {
      val origValidNames = validNames
      listOfDefinableNames(limit = maxDefns, arityLimit = 3)
        .andThen { names =>
          given validNames: ValidNames = names.foldLeft(origValidNames)((acc, pair) => acc.add(pair._1, info = ValidNames.Def(arity = pair._2)))
          names.foldMapA {
            case (name, arity) =>
              val origValidNames = validNames
              definableName
                .replicateA(arity)
                .filterDistinct
                .andThen { params =>
                  given validNames: ValidNames = params.foldLeft(origValidNames)(_.add(_, info = ValidNames.Val))
                  anyOf[Ps[Statement.Block]].map { body =>
                    List(Ps(Definition(Ps(name), params.map(name => Ps(DefParam.Name(name))), body)))
                  }
                }
          }
        }
    }

    given correctlyScopedBinding(using validNames: ValidNames): Generator[Binding.Call] =
      anyFromSeq(validNames.defNames).andThen { name =>
        anyOf[Ps[Expression]].replicateA(validNames.arity(name)).map { args =>
          Binding.Call(Ps(Path.Name(name)), args)
        }
      }

    given correctlyScopedCall(using validNames: ValidNames): Generator[Statement.Call] =
      anyOf[Ps[Binding.Call]].map(binding => Statement.Call(binding))

    private def nonScopingStmt(using ValidNames): Generator[Ps[Statement]] =
      anySumOfShape[Statement]
        .excluding[Statement.Let]
        .excluding[Statement.Var]
        .summon
        .map(Ps(_))

    given correctlyScopedStmts(using validNames: ValidNames): Generator[List[Ps[Statement]]] = {
      def impl(remaining: Int)(using validNames: ValidNames): Generator[List[Ps[Statement]]] =
        if(remaining == 0) {
          pure(Nil)
        } else {
          pure(Nil)
          | lzy {
            consOf(nonScopingStmt, impl(remaining = remaining - 1))
            | allNames
                .foldMap { name =>
                  val binderGen =
                    anyOf[Ps[Binding]].map(binding => Ps(Statement.Let(Ps(name), binding)))
                    | anyOf[Ps[Binding]].map(binding => Ps(Statement.Var(Ps(name), binding)))

                  locally {
                    given ValidNames = validNames.add(name, info = ValidNames.Val)
                    consOf(binderGen, impl(remaining = remaining - 1))
                  }
                }
          }
        }

      impl(remaining = 2)
    }
  }

  test("untargeted") {
    generalChecks(requiredReferences = 1, requiredErrors = 1, requiredDefinitions = 1) {
      given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()
      // very short lists or we're going to take forever
      given limitedList[T](using gen: Generator[T]): Generator[List[T]] =
        listOf(gen, limit = 2)
      given strGen: Generator[String] = anyFromSeq(List("foo", "bar", "ping"))
      anyOf[Module]
    }
  }

  test("mostly names: 1 definition") {
    generalChecks(requiredReferences = 3, requiredErrors = 3, requiredDefinitions = 1) {
      given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()

      val gens = new NameTargetedGens(definitionCount = 1)
      import gens.given
      anyOf[Module]
    }
  }

  test("mostly names: 3 definitions") {
    generalChecks(requiredReferences = 2, requiredErrors = 2, requiredDefinitions = 3) {
      given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()

      val gens = new NameTargetedGens(definitionCount = 3)
      import gens.given
      anyOf[Module]
    }
  }

  test("correct scoping: one defn, deeper") {
    given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()
    val gens = new CorrectlyScopedGens(maxDefns = 1)
    given ValidNames = ValidNames.empty

    import Checker.*
    given involvesStr: Transform[String, Involves] = Transform.fromFunction(_ => Monoid[Involves].empty)
    given involvesBigInt: Transform[BigInt, Involves] = Transform.fromFunction(_ => Monoid[Involves].empty)
    import Transform.given Transform[List[?], ?]

    import gens.given
    anyOf[Module]
      .toChecker
      .withPrintExamples(printExamples = false)
      .exists(_.definitions.exists(_.value.params.size >= 1))
      .exists { module =>
        // contains at least 2 sequential stmts
        module
          .involves[List[Ps[Statement]]](_.size >= 2)
          .exists
      }
      .transform { module =>
        val (info, ()) = Scoping.scopeModule(module)(using Scoping.ScopingContext.empty).run.value
        (module, info)
      } { checker =>
        checker
          .exists(_._2.referencePairs.size >= 3)
          .forall {
            case (module, info) =>
              val referencePairs = info.referencePairs.toList
              val errors = info.errors.toList

              assertEquals(errors, Nil)
          }
      }
      .run()
  }

  test("correct scoping: >=3 defns, shallow") {
    given dummyLocSrc: DummyLocSrc = IncreasingDummyLocSrc()
    val gens = new CorrectlyScopedGens(maxDefns = 3)
    given ValidNames = ValidNames.empty

    import gens.given
    anyOf[Module]
      .toChecker
      .withPrintExamples(printExamples = false)
      .exists(_.definitions.exists(_.value.params.size >= 1))
      .exists(_.definitions.size >= 3)
      .transform { module =>
        val (info, ()) = Scoping.scopeModule(module)(using Scoping.ScopingContext.empty).run.value
        (module, info)
      } { checker =>
        checker
          .forall {
            case (module, info) =>
              val referencePairs = info.referencePairs.toList
              val errors = info.errors.toList

              assertEquals(errors, Nil)
          }
      }
      .run()
  }
}
