package distcompiler

import scala.collection.mutable

trait Namespace(using ctx: NamespaceCtx):
  protected given ns: Namespace = this
  private val registeredNames = mutable.HashSet.empty[String]

  def checkName(name: String): Unit =
    require(!registeredNames(name))
    registeredNames.add(name)

  @scala.annotation.tailrec
  final def withPrefix(name: String): String =
    ctx.withPrefix(s"$tailName.$name")

  final def fullName: String =
    ctx.withPrefix(tailName)

  def tailName: String
end Namespace

transparent trait NamespaceObj extends Namespace:
  self: Singleton & Product =>

  final override def tailName: String = productPrefix
end NamespaceObj

opaque type NamespaceCtx = Namespace | Unit

object NamespaceCtx:
  def none: NamespaceCtx = ()

  inline given find: NamespaceCtx =
    scala.compiletime.summonFrom {
      case ns: Namespace => ns
      case _             => ()
    }

  extension (self: NamespaceCtx)
    inline def withPrefix(name: String): String =
      self match
        case _: Unit       => name
        case ns: Namespace => ns.withPrefix(name)
end NamespaceCtx
