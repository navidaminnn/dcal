package distcompiler.util

import scala.deriving.Mirror

opaque type NameOf[T] = String

object NameOf {
  given fromMirror[T](using
      mirror: Mirror.Of[T],
      labelValue: ValueOf[mirror.MirroredLabel]
  ): NameOf[T] = labelValue.value

  extension [T](self: NameOf[T]) def value: String = self
}
