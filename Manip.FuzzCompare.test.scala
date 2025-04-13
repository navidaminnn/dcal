// Copyright 2024-2025 DCal Team
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package distcompiler

import distcompiler.dsl.*

import distcompiler.util.FuzzTestSuite

import com.pholser.junit.quickcheck.generator.Generator
import com.pholser.junit.quickcheck.generator.GenerationStatus
import com.pholser.junit.quickcheck.random.SourceOfRandomness
import com.pholser.junit.quickcheck.generator.InRange
import com.pholser.junit.quickcheck.From

import org.junit.runner.RunWith
import edu.berkeley.cs.jqf.fuzz.JQF
import edu.berkeley.cs.jqf.fuzz.Fuzz

import scala.util.Using

@RunWith(classOf[JQF])
final class ManipFuzzCompareTests extends FuzzTestSuite:
  import ManipFuzzCompareTests.*
  // fuzzTestMethod(fuzzManipPerform)

  import org.junit.Assume
  @Fuzz
  def fuzzManipPerform(@From(classOf[ManipGenerator]) manip: Manip[?]): Unit =
    Using.resource(ManipReferenceTracer(manip)): tracer =>
      instrumentWithTracer(tracer):
        try manip.perform()
        catch
          case bt: Manip.UnrecoveredBacktrackException =>
            // these are fine but kinda trivial
            Assume.assumeTrue(false)

object ManipFuzzCompareTests:
  final class ManipGenerator extends Generator[Manip[Any]](classOf[Manip[Any]]):
    private var min = 1
    private var max = 20

    def configure(range: InRange): Unit =
      if range.min().nonEmpty
      then min = range.min().toInt
      if range.max().nonEmpty
      then min = range.max().toInt

    def generate(
        random: SourceOfRandomness,
        status: GenerationStatus
    ): Manip[Any] =
      require(min >= 0 && max >= min, s"failed 0 <= $min <= $max")
      val treeSize = random.nextInt(min, max)
      ManipGenerator.generateAny(treeSize)(using ManipGenerator.Ctx(random))

  object ManipGenerator:
    import scala.compiletime.summonAll
    import scala.deriving.Mirror
    import Manip.*

    object SimRefs:
      object ref1 extends Ref[Any]
      object ref2 extends Ref[Any]
      object ref3 extends Ref[Any]
      val refs = Array(ref1, ref2, ref3)

      def pickOne(using ctx: Ctx): Ref[Any] =
        refs(ctx.random.nextInt(refs.length))

    final class Ctx(
        val random: SourceOfRandomness,
        val requireFn: Boolean = false
    ):
      def withRequireFn: Ctx =
        Ctx(random, requireFn = true)

      def withoutRequireFn: Ctx =
        Ctx(random, requireFn = false)

      def getValue(): Any =
        val num = random.nextInt()
        def fn: Any => Any = {
          case n: Int       => num + n
          case fn: (? => ?) => fn
          case _: Tracer    => num
          case _: RefMap    => num
        }
        if requireFn then fn
        else
          random.nextBoolean() match
            case true  => num
            case false => fn

    sealed abstract class ManipGenImpl[T]:
      def shouldSkip = false
      def fixedResultType = false
      val minTreeSize: Int
      def generate(treeSize: Int)(using ctx: Ctx): T

    def generateAny(treeSize: Int)(using ctx: Ctx): Manip[Any] =
      require(treeSize >= 0, s"tree size $treeSize became negative")
      var generators =
        if treeSize >= generatorsByMinTreeSize.length
        then generatorsByMinTreeSize.last
        else generatorsByMinTreeSize(treeSize)
      if ctx.requireFn
      then generators = generators.filterNot(_.fixedResultType)

      val genIdx = ctx.random.nextInt(generators.length)
      generators(genIdx).generate(treeSize)

    private def splitSubTreeSize(subTreeSize: Int)(using ctx: Ctx): (Int, Int) =
      val lSize = ctx.random.nextInt(1, subTreeSize - 1)
      val rSize = subTreeSize - lSize
      assert(lSize >= 1)
      assert(rSize >= 1)
      (lSize, rSize)

    given ManipGenImpl[Backtrack] with
      val minTreeSize = 1
      def generate(treeSize: Int)(using ctx: Ctx): Backtrack =
        Backtrack(DebugInfo())

    given ManipGenImpl[Pure[Any]] with
      val minTreeSize = 1
      def generate(treeSize: Int)(using ctx: Ctx): Pure[Any] =
        val v = ctx.getValue()
        Pure(v)

    given ManipGenImpl[Ap[Any, Any]] with
      val minTreeSize = 3
      def generate(treeSize: Int)(using ctx: Ctx): Ap[Any, Any] =
        val (subTreeSizeL, subTreeSizeR) = splitSubTreeSize(treeSize - 1)
        val left = generateAny(subTreeSizeL)(using ctx.withRequireFn)
        Ap(left.asInstanceOf[Manip[Any => Any]], generateAny(subTreeSizeR))

    given ManipGenImpl[MapOpt[Any, Any]] with
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): MapOpt[Any, Any] =
        val v = ctx.getValue()
        MapOpt(generateAny(treeSize - 1), _ => v)

    given ManipGenImpl[FlatMap[Any, Any]] with
      val minTreeSize = 3
      def generate(treeSize: Int)(using ctx: Ctx): FlatMap[Any, Any] =
        val (subTreeSizeL, subTreeSizeR) = splitSubTreeSize(treeSize - 1)
        val result1 = generateAny(subTreeSizeR)
        val result2 = generateAny(subTreeSizeR)
        FlatMap(
          generateAny(subTreeSizeL)(using ctx.withoutRequireFn),
          {
            case _: Int      => result1
            case _: (? => ?) => result2
            case _: RefMap   => result1
            case _: Tracer   => result2
          }
        )

    given ManipGenImpl[Restrict[Any, Any]] with
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): Restrict[Any, Any] =
        val max = ctx.random.nextInt()
        val keepFn = ctx.random.nextBoolean()
        Restrict(
          generateAny(treeSize - 1),
          {
            case num: Int if num <= max   => num
            case fn: (? => ?) if keepFn   => fn
            case tracer: Tracer if keepFn => tracer
            case refMap: RefMap if keepFn => refMap
          },
          DebugInfo()
        )

    given ManipGenImpl[Effect[Any]] with
      val minTreeSize = 1
      def generate(treeSize: Int)(using ctx: Ctx): Effect[Any] =
        val v1 = ctx.getValue()
        if ctx.random.nextBoolean()
        then Effect(() => v1)
        else
          val v2 = ctx.getValue()
          var alt = false
          Effect: () =>
            alt = !alt
            if alt then v1 else v2

    given ManipGenImpl[Finally[Any]] with
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): Finally[Any] =
        Finally(
          generateAny(treeSize - 1),
          () => ()
        )

    given ManipGenImpl[KeepLeft[Any]] with
      val minTreeSize = 3
      def generate(treeSize: Int)(using ctx: Ctx): KeepLeft[Any] =
        val (subTreeSizeL, subTreeSizeR) = splitSubTreeSize(treeSize - 1)
        KeepLeft(
          generateAny(subTreeSizeL),
          generateAny(subTreeSizeR)(using ctx.withoutRequireFn)
        )

    given ManipGenImpl[KeepRight[Any]] with
      val minTreeSize = 3
      def generate(treeSize: Int)(using ctx: Ctx): KeepRight[Any] =
        val (subTreeSizeL, subTreeSizeR) = splitSubTreeSize(treeSize - 1)
        KeepRight(
          generateAny(subTreeSizeL)(using ctx.withoutRequireFn),
          generateAny(subTreeSizeR)
        )

    given ManipGenImpl[Commit[Any]] with
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): Commit[Any] =
        Commit(generateAny(treeSize - 1), DebugInfo())

    given ManipGenImpl[RefInit[Any, Any]] with
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): Manip.RefInit[Any, Any] =
        val v = ctx.getValue()
        RefInit(
          SimRefs.pickOne,
          () => v,
          generateAny(treeSize - 1),
          DebugInfo()
        )

    given ManipGenImpl[RefGet[Any]] with
      // not technically true, but because we can't prove we stored a fn,
      // assume we don't properly control getting it back
      override val fixedResultType = true
      val minTreeSize = 1
      def generate(treeSize: Int)(using ctx: Ctx): RefGet[Any] =
        require(!ctx.requireFn)
        RefGet(SimRefs.pickOne, DebugInfo())

    given ManipGenImpl[RefReset[Any, Any]] with
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): RefReset[Any, Any] =
        RefReset(SimRefs.pickOne, generateAny(treeSize - 1), DebugInfo())

    given ManipGenImpl[RefUpdated[Any, Any]] with
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): RefUpdated[Any, Any] =
        val v = ctx.getValue()
        RefUpdated(
          SimRefs.pickOne,
          (_ => v),
          generateAny(treeSize - 1),
          DebugInfo()
        )

    given ManipGenImpl[GetRefMap.type] with
      override val fixedResultType = true
      val minTreeSize = 1
      def generate(treeSize: Int)(using ctx: Ctx): GetRefMap.type =
        require(!ctx.requireFn)
        GetRefMap

    given ManipGenImpl[GetTracer.type] with
      override val fixedResultType = true
      val minTreeSize = 1
      def generate(treeSize: Int)(using ctx: Ctx): GetTracer.type =
        require(!ctx.requireFn)
        GetTracer

    given ManipGenImpl[Disjunction[Any]] with
      val minTreeSize = 3
      def generate(treeSize: Int)(using ctx: Ctx): Disjunction[Any] =
        val (subTreeSizeL, subTreeSizeR) = splitSubTreeSize(treeSize - 1)
        Disjunction(
          generateAny(subTreeSizeL),
          generateAny(subTreeSizeR),
          DebugInfo()
        )

    given ManipGenImpl[Deferred[Any]] with
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): Deferred[Any] =
        val deferred = generateAny(treeSize - 1)
        Deferred(() => deferred)

    given ManipGenImpl[TapEffect[Any]] with
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): TapEffect[Any] = ???

    given ManipGenImpl[RestrictHandle[Any]] with
      // explicit handle manipulation, simulating that is TODO
      // (idea: keep a couple of token handles, allow RefInit to set it, and then update it here)
      override val shouldSkip = true
      val minTreeSize = 2
      def generate(treeSize: Int)(using ctx: Ctx): RestrictHandle[Any] = ???

    private inline def findGenOptions(using
        mirror: Mirror.Of[Manip[Any]]
    ): Array[ManipGenImpl[Manip[Any]]] =
      summonAll[Tuple.Map[mirror.MirroredElemTypes, ManipGenImpl]].toArray
        .map(_.asInstanceOf[ManipGenImpl[Manip[Any]]])

    val generatorsByMinTreeSize: Array[Array[ManipGenImpl[Manip[Any]]]] =
      locally:
        val generators = findGenOptions
        val builder = Array.newBuilder[Array[ManipGenImpl[Manip[Any]]]]
        var lastAddition = generators.filter(_.minTreeSize <= 1)
        builder += lastAddition
        var minTreeSize = 1
        var proposedAddition = generators.filter(_.minTreeSize <= minTreeSize)
        while proposedAddition.length > lastAddition.length do
          builder += proposedAddition
          minTreeSize += 1
          lastAddition = proposedAddition
          proposedAddition = generators.filter(_.minTreeSize <= minTreeSize)
        builder.result()
    end generatorsByMinTreeSize
  end ManipGenerator
end ManipFuzzCompareTests
