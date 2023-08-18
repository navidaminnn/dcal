package test.com.github.distcompiler.dcal.chungus

import scala.collection.mutable
import scala.quoted.*

object DumpCode {
  def impl[T](expr: Expr[T])(using quotes: Quotes): Expr[T] = {
    import quotes.reflect.*

    val builder = mutable.StringBuilder()
    var atLineStart = true
    var indentNum = 0

    def indent() = {
      indentNum += 1
    }
    def dedent() = {
      assert(indentNum >= 1)
      indentNum -= 1
    }
    def put(str: String) = {
      if(atLineStart) {
        atLineStart = false
        (0 until indentNum).foreach(_ => builder ++= "| ")
      }
      builder ++= str
    }
    def newLine() = {
      atLineStart = true
      builder += '\n'
    }

    def commaSep[T](list: List[T], fn: T => Unit): Unit = {
      var atStart = true
      list.foreach { v =>
        if(atStart) {
          atStart = false
        } else {
          put(", ")
        }
      }
    }

    def tryToShowTerm(term: Term): Unit =
      term match {
        case Inlined(_, bindings, term) =>
          if(bindings.nonEmpty) {
            put("inlined {")
            indent()
            newLine()
            bindings.foreach { defn =>
              tryToShowStmt(defn)
              newLine()
            }
            dedent()
            put("}")
          } else {
            tryToShowTerm(term)
          }
        case Ident(id) => put(id)
        case Literal(const) =>
          put(const.show(using Printer.ConstantCode))
        case Select(src, id) =>
          tryToShowTerm(src)
          put(s".$id")
        case This(None) =>
          put("this")
        case This(Some(name)) =>
          put(s"$name.this")
        case Apply(fn, Nil) =>
          tryToShowTerm(fn)
          put("()")
        case Apply(fn, List(arg)) =>
          tryToShowTerm(fn)
          put("(")
          tryToShowTerm(arg)
          put(")")
        case Apply(fn, args) =>
          tryToShowTerm(fn)
          put("(")
          newLine()
          indent()
          args.foreach { v =>
            tryToShowTerm(v)
            put(",")
            newLine()
          }
          dedent()
          put(")")
        case TypeApply(term, args) =>
          tryToShowTerm(term)
          put("[")
          var first = true
          args.foreach { tpe =>
            if(first) {
              first = false
            } else {
              put(",")
            }
            put(tpe.tpe.show(using Printer.TypeReprShortCode))
            //newLine()
          }
          put("]")
        case New(tpe) =>
          put("new ")
          put(tpe.tpe.show(using Printer.TypeReprShortCode))
          //tryToShowTypeTree(tpe)
        case Closure(name, _) =>
          put("closure ")
          tryToShowTerm(name)
        case Block(Nil, v) =>
          tryToShowTerm(v)
        case Block(stmts, v) =>
          put("{")
          newLine()
          indent()
          stmts.foreach { stmt =>
            tryToShowStmt(stmt)
            newLine()
          }
          tryToShowTerm(v)
          newLine()
          dedent()
          put("}")
        case Match(head, cases) =>
          tryToShowTerm(head)
          put(" match {")
          indent()
          newLine()
          cases.foreach {
            case CaseDef(pattern, condOpt, body) =>
              put("case ")
              put(pattern.show(using Printer.TreeShortCode))
              condOpt.foreach { cond =>
                put(" if ")
                tryToShowTerm(cond)  
              }
              put(" => ")
              indent()
              newLine()
              tryToShowTerm(body)
              dedent()
              newLine()
          }
          dedent()
          put("}")
        case Typed(term, _) =>
          tryToShowTerm(term)
        case _ =>
          report.error(s"${builder.result()}\n\nTODO: $term")
      }

    // def tryToShowTypeTree(typeTree: TypeTree): Unit =
    //   typeTree.tpe match {
    //     case TypeRef(_, name) =>
    //       put(name)
    //     case _ => 
    //       report.error(s"${builder.result()}\n\nTODO: $typeTree")
    //   }

    def tryToShowStmt(stmt: Statement): Unit =
      stmt match {
        case ClassDef(name, constructor, supers, selfOpt, defns) =>
          put("class ")
          put(name)
          put(" {")
          selfOpt.foreach {
            case ValDef(name, _, _) =>
              put(s" $name =>")
          }
          newLine()
          indent()
          defns.foreach { defn =>
            tryToShowStmt(defn)
            newLine()
          }
          dedent()
          put("}")
        case ValDef(name, tpe, valOpt) =>
          put("val ")
          put(name)
          put(": ")
          put(tpe.tpe.show(using Printer.TypeReprShortCode))
          valOpt.foreach { v =>
            put(" = ")
            tryToShowTerm(v)
          }
        case TypeDef(name, tree) =>
          put(s"type $name = ?")
        case DefDef(name, paramss, tpe, bodyOpt) =>
          put("def ")
          put(name)
          paramss
            .iterator
            .foreach {
              case TypeParamClause(params) =>
                put(params.iterator.map(_.name).mkString("[", ",", "]"))
              case TermParamClause(params) =>
                put(params.iterator.map(_.name).mkString("(", ",", ")"))
            }
          bodyOpt.foreach { body =>
            put(" = ")
            tryToShowTerm(body)
          }
        case term: Term =>
          tryToShowTerm(term)
        case _ =>
          report.error(s"${builder.result()}\n\nTODO: $stmt")
      }

    tryToShowTerm(expr.asTerm)
    report.info(builder.result(), expr)
    //report.info(expr.asTerm.show(using Printer.TreeStructure), expr)
    // val summonedTree =
    //   expr match {
    //     case '{ (${_}: AnySumOfShape[?, ?]).summon(using $arg) } => arg
    //     case '{ anyOf(using $arg) } => arg
    //     case _ =>
    //       report.errorAndAbort("couldn't find summoned tree", expr)
    //   }
    //report.info(expr.asTerm.show(using Printer.TreeShortCode), expr)

    expr
  }
}
