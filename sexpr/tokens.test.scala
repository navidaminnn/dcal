package distcompiler.sexpr

class tokensTest extends munit.FunSuite:
  test("token names"):
    assertEquals(tokens.Atom.name, "distcompiler.sexpr.tokens.Atom")
    assertEquals(tokens.List.name, "distcompiler.sexpr.tokens.List")
