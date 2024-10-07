package distcompiler

class ManipTests extends munit.FunSuite:
  import ManipTests.*

  def repeat(breadth: Int, iterFn: () => Iterator[Node]): Iterator[List[Node]] =
    breadth match
      case 0 => Iterator.single(Nil)
      case _ =>
        for
          hd <- iterFn()
          tl <- repeat(breadth - 1, iterFn)
        yield hd.clone() :: tl

  def exampleNodes(depth: Int): Iterator[Node] =
    depth match
      case 0 => Iterator(tok1())
      case _ =>
        for
          breadth <- (0 until 3).iterator
          children <- repeat(breadth, () => exampleNodes(depth - 1))
        yield tok1(children)

  def examples(depth: Int): Iterator[Node.Top] =
    for
      breadth <- (0 until 3).iterator
      children <- repeat(breadth, () => exampleNodes(depth - 1))
    yield Node.Top(children)

  test("no-op top-down once traversal"):
    import dsl.*
    val ps = pass(once = true).rules(backtrack)

    examples(3).foreach: tree =>
      val orig = tree.clone()
      ps.perform(tree)
      assertEquals(tree, orig)

  test("no-op top-down traversal"):
    import dsl.*
    val ps = pass(once = false).rules(backtrack)

    examples(3).foreach: tree =>
      val orig = tree.clone()
      ps.perform(tree)
      assertEquals(tree, orig)

  test("no-op bottom-up once traversal"):
    import dsl.*
    val ps = pass(strategy = pass.bottomUp, once = true).rules(backtrack)

    examples(3).foreach: tree =>
      val orig = tree.clone()
      ps.perform(tree)
      assertEquals(tree, orig)

object ManipTests:
  object tok1 extends Token
