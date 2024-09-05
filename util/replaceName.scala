package distcompiler.util

final class ReplaceNameBuilder[T](private val value: T) extends AnyVal {
  inline def apply[U](fn: T => U): U =
    fn(value)
}

inline def replaceName[T](value: T): ReplaceNameBuilder[T] =
  ReplaceNameBuilder(value)
