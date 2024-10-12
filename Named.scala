package distcompiler

import scala.annotation.constructorOnly
import scala.quoted.*

transparent trait Named(using ownName: Named.OwnName @constructorOnly):
  final val name: String = ownName.segments.mkString(".")
  final val nameSegments: List[String] = ownName.segments
end Named

object Named:
  final class OwnName(val segments: List[String]) extends AnyVal

  private def findOwnNameImpl(using quotes: Quotes): Expr[OwnName] =
    import quotes.reflect.*

    @scala.annotation.tailrec
    def stripMacroConstructorStuff(sym: Symbol): TypeRepr =
      if sym.flags.is(Flags.Macro) || sym.isClassConstructor
      then stripMacroConstructorStuff(sym.owner)
      else if sym.isClassDef && sym.companionModule.exists
      then
        val symTermRef = sym.companionModule.termRef
        if symTermRef.isSingleton
        then symTermRef
        else report.errorAndAbort(s"${symTermRef.show} is not a singleton")
      else
        report.errorAndAbort(
          s"${sym.fullName} is not a class/object, or has no companion object"
        )

    def getNameSegments(sym: Symbol): List[String] =
      if sym == defn.RootClass
      then Nil
      // Not sure about stripSuffix, but it seems to work well, I guess until a package
      // has non-alphanumeric chars in its name.
      // Not sure how else I might try to access the name without the $.
      else sym.name.stripSuffix("$") :: getNameSegments(sym.owner)

    val theType = stripMacroConstructorStuff(Symbol.spliceOwner)
    val nameSegments = getNameSegments(theType.typeSymbol).reverse

    '{ OwnName(${ Expr(nameSegments) }) }

  inline given findOwnName: OwnName =
    ${ findOwnNameImpl }
end Named
