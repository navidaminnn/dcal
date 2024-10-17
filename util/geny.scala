package distcompiler.util

extension (lhs: geny.Writable)
  @scala.annotation.targetName("writableConcat")
  def ++(rhs: geny.Writable): geny.Writable =
    new geny.Writable:
      def writeBytesTo(out: java.io.OutputStream): Unit =
        lhs.writeBytesTo(out)
        rhs.writeBytesTo(out)
