package distcompiler.transform

import cats.*
import cats.syntax.all.given

final case class Count(count: Int, depth: Int) derives CanEqual {
  def combine(other: Count): Count =
    Count(
      count = count + other.count,
      depth = depth `max` other.depth,
    )

  def inc: Count =
    Count(
      count = count + 1,
      depth = depth + 1,
    )

  def exists: Boolean = count > 0
}

object Count {
  val empty: Count = Count(0, 0)
  val one: Count = Count(1, 1)

  def fromBoolean(cond: Boolean): Count =
    if(cond) one else empty

  given monoid: Monoid[Count] with {
    def combine(x: Count, y: Count): Count = x.combine(y)
    def empty: Count = Count.empty
  }
}
