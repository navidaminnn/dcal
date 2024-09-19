package distcompiler

import scala.collection.{mutable, MapView}
import scala.reflect.Typeable

trait Token extends Equals, Named, NamespaceRequired:
  final override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Token]

  final override def equals(that: Any): Boolean =
    this eq that.asInstanceOf[AnyRef]

  final override def hashCode(): Int =
    System.identityHashCode(this)

  final def canBeLookedUp: Boolean = lookedUpBy.nonEmpty

  def symbolTableFor: List[Token] = Nil
  def lookedUpBy: Option[Node => Option[Node]] = None
  def showSource: Boolean = false
end Token

object Token:
  extension (token: Token)(using ctx: Wellformed)
    @scala.annotation.alpha("assignShape")
    def ::=(shape: Shape): Unit =
      require(ctx.isBuilding)
      ctx.shapeMapBuilder(token) = shape

    def modifyShape(fn: Shape => Shape): Unit =
      require(ctx.isBuilding)
      ctx.shapeMapBuilder.updateWith(token): shapeOpt =>
        require(shapeOpt.nonEmpty)
        Some(fn(shapeOpt.get))

  extension (token: Token)
    def apply(children: Node.Child*): Node =
      Node(token)(children)
    def apply(children: IterableOnce[Node.Child]): Node =
      Node(token)(children)
end Token

trait TokenObj(shape: TokenObj.ShapeSrc) extends Token, NamedObj:
  self: Singleton & Product =>
  locally:
    import TokenObj.ShapeSrc
    shape match
      case ShapeSrc.None => // skip
      case ShapeSrc.Immediate(shape) =>
        require(namespaceCtx.isInstanceOf[Wellformed])
        given Wellformed = namespaceCtx.asInstanceOf[Wellformed]
        this ::= shape
      case ShapeSrc.Deferred(shapeFn) =>
        require(namespaceCtx.isInstanceOf[Wellformed])
        given wf: Wellformed = namespaceCtx.asInstanceOf[Wellformed]
        wf.deferBuildStep:
          this ::= shapeFn()
end TokenObj

object TokenObj:
  enum ShapeSrc:
    case None
    case Immediate(shape: Shape)
    case Deferred(shapeFn: () => Shape)
  end ShapeSrc

  object ShapeSrc:
    given immediateShape: Conversion[Shape, ShapeSrc] = ShapeSrc.Immediate.apply
    given immediateEmbed[T]: Conversion[Shape.Embed[T], ShapeSrc] with
      def apply(embed: Shape.Embed[T]): ShapeSrc = immediateShape(
        Shape.Choice(Set(embed))
      )
  end ShapeSrc
end TokenObj

trait Wellformed extends Namespace:
  import Wellformed.*
  protected given wf: Wellformed = this

  private[distcompiler] val shapeMapBuilder =
    mutable.HashMap.empty[Token, Shape]
  private[distcompiler] var deferredOps: mutable.ListBuffer[() => Unit] | Null =
    mutable.ListBuffer.empty

  // Make it "just work" when someone uses built-in nodes re: well-formedness checks.
  basedOn(Builtin)

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

trait WellformedObj(using NamespaceCtx) extends Wellformed, NamespaceObj:
  self: Singleton & Product =>
end WellformedObj

sealed trait Shape

object Shape:
  extension (shape: => Shape)
    def defer: TokenObj.ShapeSrc =
      TokenObj.ShapeSrc.Deferred(() => shape)

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

case object Builtin extends WellformedObj:
  import Shape.*

  case object top extends TokenObj(AnyShape)

  case object tuple extends TokenObj(AnyShape)

  case object error extends TokenObj(Fields(errorMsg, errorAST)):
    def apply(msg: String, ast: Node.Child): Node =
      error(
        errorMsg().at(Source(msg)),
        errorAST(ast)
      )
  end error
  case object errorMsg extends TokenObj(Atom):
    override val showSource = true
  case object errorAST extends TokenObj(AnyShape)

  case object sourceMarker extends TokenObj(Atom)

  case object lift extends TokenObj(Fields(liftDest, liftNode, origNode)):
    def apply(dest: Token, node: Node, nodeInPlace: Node): Node =
      lift(
        liftDest(dest()),
        liftNode(node),
        origNode(nodeInPlace)
      )
  end lift
  case object liftDest extends TokenObj(Embed[Token])
  case object liftNode extends TokenObj(AnyShape)
  case object origNode extends TokenObj(AnyShape)
end Builtin
