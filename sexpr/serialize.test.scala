package distcompiler.sexpr

import java.io.ByteArrayOutputStream

import distcompiler.*
import utest.*

object serializeTests extends TestSuite:
  def tests = Tests:
    extension (writable: geny.Writable)
      def writeToString: String =
        val out = ByteArrayOutputStream()
        writable.writeBytesTo(out)
        out.toString()

    extension (top: Node.Top)
      def serializeCompact: String =
        serialize.toCompactWritable(top).writeToString
      def serializePretty: String =
        serialize.toPrettyWritable(top).writeToString

    val eg1 = Node.Top(tokens.List(tokens.Atom("foo"), tokens.Atom("bar")))

    test("eg1 compact"):
      eg1.serializeCompact ==> "(3:foo3:bar)"

    test("eg1 pretty"):
      eg1.serializePretty ==> """(3:foo
                                |  3:bar)""".stripMargin

    val eg2 = Node.Top(
      tokens.List(tokens.List(), tokens.List(), tokens.List(tokens.List()))
    )

    test("nested lists compact"):
      eg2.serializeCompact ==> "(()()(()))"

    test("nested lists pretty"):
      eg2.serializePretty ==> """(
                                |  ()
                                |  ()
                                |  (()))""".stripMargin
