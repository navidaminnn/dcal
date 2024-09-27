package distcompiler.sexpr

import distcompiler.Token

object tokens:
  object List extends Token
  object Atom extends Token:
    override def showSource: Boolean = true
  object String extends Token:
    override def showSource: Boolean = true
