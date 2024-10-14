package distcompiler

import wf.*

class WellformedTests extends munit.FunSuite:
  import WellformedTests.*
  val wf = Wellformed:
    Node.Top ::= AnyShape
    tok1 ::= AnyShape
    tok2 ::= AnyShape

  val tmpSrc = Source.mapFromFile(os.temp("foo"))
  val src1 = SourceRange.entire(tmpSrc).take(2)
  val src2 = SourceRange.entire(tmpSrc).drop(2)
  val src3 = SourceRange.entire(Source.fromString("bar"))

  def joinN(breadth: Int, iterFn: () => Iterator[Node]): Iterator[List[Node]] =
    breadth match
      case 0 => Iterator.single(Nil)
      case breadth =>
        for
          hd <- iterFn()
          tl <- joinN(breadth - 1, iterFn)
        yield hd.clone() :: tl

  def exampleNodes(depth: Int): Iterator[Node] =
    depth match
      case 0 => Iterator.empty
      case _ if depth > 0 =>
        for
          breadth <- (0 until 3).iterator
          parent <- Iterator(tok1(src1), tok2(src2), tok1(src3), tok1())
          children <- joinN(breadth, () => exampleNodes(depth - 1))
        yield locally:
          val p = parent.clone()
          p.children.addAll(children)
          p

  def examples(depth: Int): Iterator[Node.Top] =
    for
      breadth <- (0 until 3).iterator
      children <- joinN(breadth, () => exampleNodes(depth - 1))
    yield Node.Top(children)

  test("back and forth"):
    examples(3).foreach: tree =>
      import dsl.*
      val orig = tree.clone()
      val ser = wf.serializeTree(tree)
      if orig.children.nonEmpty
      then assertNotEquals(ser, orig)

      val deser = wf.deserializeTree(ser)
      assertEquals(deser, orig)

object WellformedTests:
  object tok1 extends Token:
    override def showSource: Boolean = true
  object tok2 extends Token
