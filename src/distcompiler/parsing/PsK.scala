package distcompiler.parsing

import cats.Comonad

final case class PsK[+T](value: T, sourceLocation: SourceLocation) {
  def map[U](fn: T => U): PsK[U] =
    copy(value = fn(value))

  def toPs: Ps[T] =
    Ps(value)(using sourceLocation)
}

object PsK {
  given comonad: Comonad[PsK] with {
    override def extract[A](x: PsK[A]): A = x.value

    override def map[A, B](fa: PsK[A])(f: A => B): PsK[B] =
      fa.map(f)

    override def coflatMap[A, B](fa: PsK[A])(f: PsK[A] => B): PsK[B] =
      PsK(f(fa), fa.sourceLocation)
  }
}
