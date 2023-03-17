package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.IR.Node

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Utils {
  object IRUtils {

    def delimit[T](lst: List[List[T]], delimiter: T): List[T] = {
      @tailrec
      def _delimit(lst: List[List[T]], acc: ListBuffer[T]): ListBuffer[T] =
        lst match {
          case Nil => acc
          case h :: t => t match
            case Nil => _delimit(t, acc :++ h)
            case _ => _delimit(t, acc :++ h += delimiter)
        }

      _delimit(lst, ListBuffer[T]()).toList
    }

    def stringifyNode(node: IR.Node): Iterator[Char] =
      node match {
        case Node.Name(name) => name.iterator

        case Node.Let(name, binding, body) =>
          s"LET $name == ".iterator ++
            stringifyNodes(binding) ++
            "\nIN ".iterator ++
            stringifyNodes(body) ++
            "\n".iterator

        case Node.Uninterpreted(text) => text.iterator

        case Node.MapOnSet(set, setMember, proc) =>
                    "{ ".iterator ++
                      stringifyNodes(proc) ++
                      ": ".iterator ++
                      s"$setMember \\in ".iterator ++
                      stringifyNodes(set) ++
                      " }".iterator

        case Node.FilterOnSet(set, setMember, pred) =>
          "{ ".iterator ++
            s"$setMember \\in ".iterator ++
            stringifyNodes(set) ++
            ": ".iterator ++
            stringifyNodes(pred) ++
            " }".iterator
      }

    /**
     * Converts a list of IR.Node representing TLA+ code to a String of TLA+ code
     */
    def stringifyNodes(nodes: List[IR.Node]): Iterator[Char] =
      nodes.iterator.flatMap(stringifyNode)

    def stringifyParams(params: List[String]): Iterator[Char] =
      params.mkString(", ").iterator

    def stringifyDefinition(definition: IR.Definition): Iterator[Char] =
      definition.name.iterator ++
        "(".iterator ++
        stringifyParams(definition.params) ++
        ") ==\n".iterator ++
        stringifyNodes(definition.body) ++
        "\n".iterator

    def stringifyModule(module: IR.Module): Iterator[Char] =
      s"---- MODULE ${module.name} ----".iterator ++
        "\nEXTENDS Naturals\n".iterator ++
        module.definitions.iterator.flatMap(stringifyDefinition) ++
        "====\n".iterator
  }
}
