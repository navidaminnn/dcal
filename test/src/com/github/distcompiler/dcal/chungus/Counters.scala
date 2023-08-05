package test.com.github.distcompiler.dcal.chungus

final case class Counters private (
  budget: Int,
  counters: Map[Counters.Key[?], Int],
) {
  require(budget >= 0)

  def resetBudget(budget: Int): Counters =
    copy(budget = budget)

  def countOne(key: Counters.Key[?]): Counters =
    copy(counters = counters.updated(key, counters.getOrElse(key, 0) + 1))

  def payOne: Counters = {
    assert(budget > 0)
    copy(budget = budget - 1)
  }

  def read(key: Counters.Key[?]): Int =
    counters.getOrElse(key, 0)
}

object Counters {
  trait Key[T]

  def apply(budget: Int): Counters =
    Counters(
      budget = budget,
      counters = Map.empty,
    )
}
