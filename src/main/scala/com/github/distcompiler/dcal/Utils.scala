package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.IR.Node

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Utils {
  object TopologicalSort {
    enum State {
      case UNVISITED
      case VISITING
      case VISITED
    }

    def sort(neighborsOf: Map[String, List[String]]): Either[CircularDefinition, List[String]] =
      val nodes = neighborsOf.keys
      val visited = nodes.map { n => (n -> State.UNVISITED) }.to(collection.mutable.Map)

      def dfs(node: String, ordered: List[String]): Either[CircularDefinition, List[String]] = {
        visited(node) match
          case State.UNVISITED =>
            visited(node) = State.VISITING
            neighborsOf(node).foldLeft(Right(ordered).withLeft[CircularDefinition]) { (orderedOpt, neighbor) =>
              orderedOpt match
                case err@Left(_) => err
                case Right(ordered) => dfs(neighbor, ordered)
            } match {
              case err@Left(_) => err
              case Right(rest) =>
                visited(node) = State.VISITED
                Right(node :: rest)
            }
          case State.VISITING => Left(CircularDefinition(node))
          case State.VISITED => Right(ordered)
      }

      val allOrderedOpt = nodes.foldLeft(Right(List[String]()).withLeft[CircularDefinition]) { (orderedOpt, node) =>
        orderedOpt match
          case err@Left(_) => err
          case Right(ordered) =>
            visited(node) match
              case State.UNVISITED => dfs(node = node, ordered = ordered)
              case State.VISITING => Left(CircularDefinition(node))
              case State.VISITED => orderedOpt
      }

      allOrderedOpt match {
        case err@Left(_) => err
        case Right(allOrdered) => Right(allOrdered.reverse)
      }
  }

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
     * Converts a list of IR nodes representing TLA+ code to an char iterator of TLA+ code
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
