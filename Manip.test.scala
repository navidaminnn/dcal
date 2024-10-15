package distcompiler

class ManipTests extends munit.FunSuite:
  import ManipTests.*

  def repeat(breadth: Int, iterFn: () => Iterator[Node]): Iterator[List[Node]] =
    breadth match
      case 0 => Iterator.single(Nil)
      case _ if breadth > 0 =>
        for
          hd <- iterFn()
          tl <- repeat(breadth - 1, iterFn)
        yield hd.clone() :: tl

  def exampleNodes(depth: Int, shouldVary: Boolean): Iterator[Node] =
    depth match
      case 0 => Iterator.empty
      case _ if depth > 0 =>
        for
          breadth <- (0 until 3).iterator
          parent <- Iterator(tok1(), if shouldVary then tok2() else tok1())
          children <- repeat(breadth, () => exampleNodes(depth - 1, shouldVary))
        yield locally:
          val p = parent.clone()
          p.children.addAll(children)
          p

  def examples(depth: Int, shouldVary: Boolean): Iterator[Node.Top] =
    for
      breadth <- (0 until 3).iterator
      children <- repeat(breadth, () => exampleNodes(depth - 1, shouldVary))
    yield Node.Top(children)

  def treePairs: Iterator[(Node.Top, Node.Top)] =
    examples(4, shouldVary = true).zip(examples(4, shouldVary = false))

  import dsl.*
  val unifyColors1 =
    pass(once = true)
      .rules:
        on(
          tok(tok2)
        ).rewrite: node =>
          spliceThen(tok1(node.unparentedChildren)):
            continuePassAtNextNode

  // test sanity condition
  test("all trees are equal".fail):
    treePairs.foreach: (bi, mono) =>
      assertEquals(bi, mono)

  test("treePairs unifyColors1"):
    treePairs.foreach: (bi, mono) =>
      atNode(bi)(unifyColors1).perform()
      assertEquals(bi, mono)

  test("unifyColors1 on a single node"):
    val bi = Node.Top(tok2())
    val mono = Node.Top(tok1())
    atNode(bi)(unifyColors1).perform()
    assertEquals(bi, mono)

object ManipTests:
  object tok1 extends Token
  object tok2 extends Token
