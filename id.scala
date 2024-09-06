package distcompiler

final class ById[T <: AnyRef](val ref: T) extends Equals:
  override def canEqual(that: Any): Boolean =
    that.isInstanceOf[ById[?]]

  override def equals(that: Any): Boolean =
    that match
      case that: ById[t] => ref eq that.ref
      case _             => false

  override def hashCode(): Int =
    System.identityHashCode(ref)

  override def toString(): String =
    s"ById(@${hashCode()} $ref)"
end ById

object ById:
  def unapply[T <: AnyRef](byId: ById[T]): Some[T] = Some(byId.ref)
end ById
