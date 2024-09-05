package distcompiler

import scala.collection.mutable

transparent trait Named(using protected val namespaceCtx: NamespaceCtx):
  namespaceCtx.checkName(name)

  def name: String
end Named

transparent trait NamespaceRequired(using Namespace) extends Named

trait NamedObj extends Named:
  self: Singleton & Product =>

  final override def name: String = productPrefix
end NamedObj

trait Namespace extends Named:
  protected given ns: Namespace = this
  private val registeredNames = mutable.HashSet.empty[String]

  def checkName(name: String): Unit =
    require(!registeredNames(name))
    registeredNames.add(name)

  @scala.annotation.tailrec
  final def withPrefix(nameToPrefix: String): String =
    namespaceCtx.withPrefix(s"$name.$nameToPrefix")

  final def fullName: String =
    namespaceCtx.withPrefix(name)

  def name: String
end Namespace

transparent trait NamespaceObj extends Namespace, NamedObj:
  self: Singleton & Product =>
end NamespaceObj

opaque type NamespaceCtx = Namespace | Unit

object NamespaceCtx:
  val none: NamespaceCtx = ()

  opaque type Find = NamespaceCtx

  inline given apply: NamespaceCtx =
    scala.compiletime.summonFrom:
      case ns: Namespace => ns
      case _             => ()

  extension (self: NamespaceCtx)
    inline def checkName(name: String): Unit =
      self match
        case _: Unit => // pass
        case ns: Namespace => ns.checkName(name)

    inline def withPrefix(name: String): String =
      self match
        case _: Unit       => name
        case ns: Namespace => ns.withPrefix(name)
end NamespaceCtx
