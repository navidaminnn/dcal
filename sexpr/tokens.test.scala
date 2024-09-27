package distcompiler.sexpr

import utest.*

object tokensTest extends TestSuite:
  def tests = Tests {
    test("token names"):
      tokens.Atom.name ==> "distcompiler.sexpr.tokens.Atom"
      tokens.List.name ==> "distcompiler.sexpr.tokens.List"
  }
