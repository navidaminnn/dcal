package distcompiler.util

final class Lazy[+T] private (valueImpl: => T) {
  lazy val value: T = valueImpl
}

object Lazy {
  def apply[T](value: => T): Lazy[T] = new Lazy(value)
}
