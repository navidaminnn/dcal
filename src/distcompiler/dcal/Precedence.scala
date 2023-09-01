package distcompiler.dcal

import cats.*
//import cats.syntax.all.given

final case class Precedence(min: Int, max: Int) derives CanEqual

object Precedence {
  val max: Precedence = Precedence(Int.MaxValue, Int.MaxValue)
  val min: Precedence = Precedence(0, 0)

  given order: PartialOrder[Precedence] = PartialOrder.from { (left, right) =>
    (left, right) match {
      case (Precedence(_, maxL), Precedence(minR, _)) if maxL < minR => -1
      case (Precedence(minL, _), Precedence(_, maxR)) if maxR < minL => 1
      case (left, right) if left == right => 0
      case _ => Double.NaN
    }
  }

  given hash: Hash[Precedence] = Hash.fromUniversalHashCode
}
