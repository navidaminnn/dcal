package test.com.github.distcompiler.dcal.chungus

import collection.mutable
import compiletime.codeOf

enum AssertCtx {
  case Empty
  case Recording[T](prefix: AssertCtx, name: String, value: () => T)
}

inline def recording[T](inline value: T)(body: AssertCtx ?=> Unit)(using ctx: AssertCtx = AssertCtx.Empty): Unit = {
  val storedValue = value
  val outerCtx = ctx
  locally {
    given AssertCtx = AssertCtx.Recording(outerCtx, codeOf(value), () => value)
    body
  }
}

private def buildAssertMsg(msg: String)(using AssertCtx): String = {
  val builder = mutable.StringBuilder()

  def impl(ctx: AssertCtx): Unit =
    ctx match {
      case AssertCtx.Empty => 
      case AssertCtx.Recording(prefix, name, value) =>
        impl(prefix)
        if(builder.nonEmpty) {
          builder += '\n'
        }
        builder ++= s">> $name = "
        pprint.tokenize(value(), initialOffset = 2)
          .map(_.render)
          .foreach(builder ++= _)
    }
    
  impl(summon[AssertCtx])
  if(builder.nonEmpty) {
    builder += '\n'
  }
  builder ++= msg
  builder.result()
}

private inline def throwAssertionError(msg: String)(using AssertCtx): Nothing =
  throw AssertionError(buildAssertMsg(msg))

inline def assert(condition: Boolean)(using ctx: AssertCtx = AssertCtx.Empty): Unit =
  if(!condition) {
    throwAssertionError(s"boolean assertion failed")
  }

inline def assertMatch[T](inline value: T)(inline fn: PartialFunction[T, Unit])(using ctx: AssertCtx = AssertCtx.Empty): Unit =
  fn.applyOrElse(value, { _ =>
    throwAssertionError(s"!! ${codeOf(value)} did not match pattern")
  })
