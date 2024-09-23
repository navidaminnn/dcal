package distcompiler

import cats.syntax.all.given
import scala.collection.{mutable, MapView}
import scala.reflect.Typeable

// TODO: heavily simplify Wellformed
// most of this structure can be written using Node, and we just provide syntax sugar

trait Wellformed:
  import Wellformed.*
  protected given wf: Wellformed = this

  private[distcompiler] val shapeMapBuilder =
    mutable.HashMap.empty[Token, Shape]
  private[distcompiler] var deferredOps: mutable.ListBuffer[() => Unit] | Null =
    mutable.ListBuffer.empty

  final def isBuilding: Boolean =
    deferredOps ne null

  final def deferBuildStep(step: => Unit): Unit =
    require(isBuilding)
    deferredOps.nn.addOne(() => step)

  final lazy val shapes: MapView[Token, Shape] =
    assert(isBuilding)
    deferredOps.nn.foreach(_.apply())
    deferredOps = null

    import Shape.*

    val queue = mutable.ArrayDeque[Token](Builtin.top)
    def enqueueChoice(choice: Choice): Unit =
      choice.choices.foreach:
        case _: Shape.Embed[?] =>
        case token: Token      => queue.addOne(token)

    while queue.nonEmpty
    do
      val token = queue.removeHead()
      require(shapeMapBuilder.contains(token))
      shapeMapBuilder(token) match
        case AnyShape         =>
        case Atom             =>
        case Fields(fields)   => fields.foreach(enqueueChoice)
        case choice: Choice   => enqueueChoice(choice)
        case Repeated(choice) => enqueueChoice(choice)
    end while

    shapeMapBuilder.view
  end shapes

  final def checkTree(top: Node.Top): Boolean =
    import Shape.*

    def checkChoice(choice: Choice, sibling: Node.Sibling): Boolean =
      sibling match
        case _: Node.RightSiblingSentinel => false
        case child: Node.Child =>
          choice.choices.exists:
            case embed: Shape.Embed[t] =>
              child match
                case _: Node                       => false
                case Node.Embed(embed.typeable(_)) => true
                case Node.Embed(_)                 => false

            case token: Token =>
              child match
                case childNode: Node => childNode.token == token
                case _: Node.Leaf    => false

    def checkParent(parent: Node.Parent, shape: Shape): Boolean =
      shape match
        case _ if parent match
              case parentNode: Node => parentNode.token == Builtin.error
              case _                => false
            =>
          true // don't check error nodes
        case AnyShape => true
        case Atom =>
          parent.children.isEmpty
        case Fields(fields) =>
          if parent.children.length != fields.length
          then false
          else
            fields.iterator
              .zip(parent.children)
              .forall(checkChoice)
        case choice: Choice =>
          checkChoice(choice, parent.firstChild)
          && parent.children.length == 1
        case Repeated(choice) =>
          parent.children.forall(checkChoice(choice, _))

    @scala.annotation.tailrec
    def impl(sibling: Node.Sibling, isOk: Boolean): Boolean =
      sibling match
        case node: Node =>
          val shapeOpt = shapes.get(node.token)
          import Shape.*

          val thisOneOk: Boolean =
            shapeOpt match
              case None =>
                // if the shape isn't specified, flag an error
                false
              case Some(shape) =>
                checkParent(node, shape)

          if thisOneOk
          then impl(node.firstChild, isOk)
          else
            val err =
              node.replaceThis:
                Builtin.error(
                  if shapeOpt.isEmpty
                  then "missing shape info"
                  else "unexpected node here",
                  node.unparent()
                )

            impl(err.rightSibling, isOk = false)

        case _: Node.RightSiblingSentinel =>
          sibling.parent match
            case parentNode: Node =>
              impl(parentNode.rightSibling, isOk)
            case _: Node.Root =>
              isOk

        case _: Node.Leaf =>
          impl(sibling.rightSibling, isOk)

    shapes.get(Builtin.top) match
      case None =>
        false
      case Some(shape) =>
        if checkParent(top, shape)
        then impl(top.firstChild, isOk = true)
        else
          top.children = List:
            Builtin.error(
              "unexpected node here",
              Builtin.tuple(top.children.iterator.map(_.unparent()))
            )
          impl(top.firstChild, isOk = false)
  end checkTree
end Wellformed

object Wellformed:
  def basedOn(using self: Wellformed)(
      other: Wellformed
  ): Unit =
    self.shapeMapBuilder.addAll(other.shapes)
end Wellformed

sealed trait Shape

object Shape:
  final class Embed[T](using val typeable: Typeable[T], val asNode: AsNode[T])

  case object AnyShape extends Shape

  case object Atom extends Shape

  final case class Fields(fields: List[Choice]) extends Shape

  object Fields:
    def apply(fields: Choice*): Fields =
      Fields(fields.toList)
  end Fields

  final case class Choice(choices: Set[Token | Embed[?]]) extends Shape:
    @scala.annotation.alpha("or")
    def |(other: Token): Choice =
      Choice(choices + other)

    @scala.annotation.alpha("orEmbed")
    def |[T](other: Embed[T]): Choice =
      Choice(choices + other)

    def extend(choices: Token*): Choice =
      Choice(this.choices ++ choices)

    def restrict(choices: Token*): Choice =
      Choice(this.choices -- choices)
  end Choice

  object Choice:
    given tokenAsChoice: Conversion[Token, Choice] with
      def apply(token: Token): Choice =
        Choice(Set(token))
  end Choice

  final case class Repeated(choice: Choice) extends Shape
end Shape

object Builtin:
  import Shape.*

  object top extends Token

  object tuple extends Token

  object error extends Token:
    def apply(msg: String, ast: Node.Child): Node =
      error(
        error.msg().at(Source(msg)),
        error.ast(ast)
      )

    object msg extends Token:
      override val showSource = true
    object ast extends Token
  end error

  object sourceMarker extends Token

  object lift extends Token:
    def apply(dest: Token, node: Node, nodeInPlace: Node): Node =
      lift(
        lift.dest(dest()),
        lift.toLift(node),
        lift.resultNode(nodeInPlace)
      )

    object dest extends Token
    object toLift extends Token
    object resultNode extends Token

    import Manip.*
    import Pattern.*
    lazy val rules: Rules =
      on(
        (tok(lift) *> firstChild(tok(lift.dest)))
          .flatMap(dest => ancestor(tok(dest.token)))
        *: children:
          (tok(lift.dest) <* children(atEnd))
            *>: tok(lift.toLift)
            **: tok(lift.resultNode)
            <*: atEnd
      ).rewrite: (destNode, liftNode, origNode) =>
        destNode.children.addOne(liftNode.unparent())
        origNode.unparent()
      | on(tok(lift))
        .rewrite: badLift =>
          error("malformed lift node", badLift.unparent())
  end lift
end Builtin
