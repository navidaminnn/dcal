package distcompiler.util

transparent trait HasInstanceArray[T](using HasInstanceArray.Instances[T]):
  val instances: IArray[T] = summon[HasInstanceArray.Instances[T]].array

object HasInstanceArray:
  class Instances[T](val array: IArray[T]) extends AnyVal

  inline given summonInstances[T: reflect.ClassTag](using mirror: deriving.Mirror.SumOf[T]): Instances[T] =
    given [S <: Singleton](using v: ValueOf[S]): S = v.value
    Instances:
      compiletime.summonAll[mirror.MirroredElemTypes]
        .toIArray
        .map(_.asInstanceOf[T])
