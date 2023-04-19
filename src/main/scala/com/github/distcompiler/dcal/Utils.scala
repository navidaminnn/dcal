package com.github.distcompiler.dcal
object Utils {
  final class TopologicalSort(neighborsOf: Map[String, List[String]]) {
    enum State {
      case UNVISITED
      case VISITING
      case VISITED
    }

    def sort: Either[CircularDefinition, List[String]] =
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
}
