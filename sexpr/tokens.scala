package distcompiler.sexpr

import distcompiler.{Token, Named, Node}

object tokens:
  object List extends Token
  object Atom extends Token:
    override def showSource: Boolean = true
