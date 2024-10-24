package distcompiler

import scala.collection.mutable
import cats.syntax.all.given
import dsl.*

transparent trait PassSeq:
  def inputWellformed: Wellformed
  final def outputWellformed: Wellformed =
    assert(entriesSealed)
    entries.last.wellformed

  private val entries = mutable.ListBuffer.empty[PassSeq.Entry]
  private var entriesSealed = false

  protected def prevWellformed(using BuildCtx): Wellformed =
    require(entries.nonEmpty, "there is not previous Wellformed")
    entries.last.wellformed

  protected object wellformed:
    def :=(using ctx: BuildCtx)(wellformed: Wellformed): Unit =
      require(ctx.wellformedOpt.isEmpty)
      ctx.wellformedOpt = Some(wellformed)

  protected final class BuildCtx:
    var wellformedOpt: Option[Wellformed] = None

  final def passDef(using DebugInfo)(
      fn: BuildCtx ?=> Manip[Unit]
  ): PassSeq.Entry =
    require(!entriesSealed, "tried to add a pass after PassSeq was constructed")
    val ctx = BuildCtx()
    val rules = fn(using ctx)

    require(ctx.wellformedOpt.nonEmpty)
    val entry = PassSeq.Entry(ctx.wellformedOpt.get, rules)
    entries += entry

    entry

  private lazy val allPasses =
    entriesSealed = true
    entries
      .foldLeft(inputWellformed.markErrorsPass): (acc, entry) =>
        acc
        *> getNode.flatMap: node =>
          if node.hasErrors
          then Manip.unit
          else
            entry.rules
              *> entry.wellformed.markErrorsPass

  final def apply(top: Node.Top, tracer: Manip.Tracer = Manip.NopTracer): Unit =
    atNode(top)(allPasses)
      .withTracer(tracer)
      .perform()

object PassSeq:
  final case class Entry(wellformed: Wellformed, rules: Manip[Unit])
