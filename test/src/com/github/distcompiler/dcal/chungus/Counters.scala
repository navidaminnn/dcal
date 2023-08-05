package test.com.github.distcompiler.dcal.chungus

final case class Counters private (
  counters: Map[Counters.Key[?], Int],
) {
  def countOne(key: Counters.Key[?]): Counters =
    copy(counters = counters.updated(key, counters.getOrElse(key, 0) + 1))

  def read(key: Counters.Key[?]): Int =
    counters.getOrElse(key, 0)
}

object Counters {
  trait Key[T]

  val init: Counters = Counters(
    counters = Map.empty,
  )
}
