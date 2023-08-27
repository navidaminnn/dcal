package distcompiler.transform

opaque type IsPrimitive[-T] = Unit

object IsPrimitive {
  def fresh[T]: IsPrimitive[T] = ()

  given numerics: IsPrimitive[Byte | Int | Long | Short | Double | Float] = fresh

  given effectivelyPrimitive: IsPrimitive[String | BigInt] = fresh
}
