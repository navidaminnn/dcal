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
    def apply(handles: Node.Child*): Node =
      Node(token, mutable.ArrayBuffer.from(handles))
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

    // TODO: sanity check, all shapes covered?

    shapeMapBuilder.view

  final def checkNode(root: Node): Boolean =
    require(root.token == Builtin.top)

    import Builtin.{error, tuple}

    @scala.annotation.tailrec
    def impl(parent: Node, idx: Int, isOk: Boolean): Boolean =
      import Shape.*

      def flagError(msg: String)(using tokenBeingChecked: Token): Unit =
        parent.children(idx) = error(
          msg,
          tuple(
            Node.Embed(tokenBeingChecked),
            parent.children(idx).reparent
          )
        )

      inline def gotoSibling(isOk: Boolean): Boolean =
        impl(parent, idx + 1, isOk)

      inline def gotoFirstChild(isOk: Boolean): Boolean =
        val child = parent.children(idx)
        assert(child.isNode)
        impl(child.asNode, 0, isOk)

      def checkChoice(parent: Node, idx: Int, choice: Choice)(using
          tokenBeingChecked: Token
      ): Boolean =
        val child = parent.children(idx)
        def checkOne(option: Token | Embed[?]): Boolean =
          option match
            case expectedToken: Token =>
              child match
                case child: Node   => child.token == expectedToken
                case Node.Embed(_) => false
            case expectedEmbed: Embed[t] =>
              child match
                case _: Node                               => false
                case Node.Embed(expectedEmbed.typeable(_)) => true
                case Node.Embed(_)                         => false

        if !choice.choices.exists(checkOne)
        then
          flagError(s"no cases matched at index $idx")
          false
        else true

      if !parent.children.isDefinedAt(idx)
      then
        assert(idx == parent.children.length)
        if parent.hasParent
        then impl(parent.parent, parent.idxInParent + 1, isOk)
        else isOk
      else
        val node = parent.children(idx)
        node match
          case node: Node =>
            import Shape.*
            given tokenBeingChecked: Token = node.token
            assert(shapes.contains(node.token))
            shapes(node.token) match
              case AnyShape =>
                impl(parent, idx + 1, isOk)
              case Atom(isLookup, isLookdown, hasScope, showSource) =>
                if node.children.length == 0
                then impl(parent, idx + 1, isOk)
                else
                  flagError("expected atom")
                  gotoSibling(isOk = false)
              case Fields(fields) =>
                if node.children.length != fields.length
                then
                  flagError("wrong number of children")
                  gotoSibling(isOk = false)
                else if node.children.indices.iterator
                    .zip(fields.iterator)
                    // This weird pattern ensures we check all the children.
                    // Yes, we'll still know if we stop early, but then we won't flag all the errors.
                    // We'll check all of them on success anyway, which is more common; there's literally no upside to reporting the error case faster.
                    .map: (idx, choice) =>
                      checkChoice(parent, idx, choice)
                    .foldLeft(true)(_ && _)
                then gotoFirstChild(isOk)
                else gotoSibling(isOk = false)
              case choice: Choice =>
                if node.children.length == 1
                then
                  if checkChoice(node, 0, choice)
                  then gotoFirstChild(isOk)
                  else gotoSibling(isOk = false)
                else
                  flagError("expected one child")
                  gotoSibling(isOk = false)
              case Repeated(choice) =>
                val parentTmp = node
                if node.children.iterator.zipWithIndex
                    .map: (node, idx) =>
                      checkChoice(parentTmp, idx, choice)
                    .foldLeft(true)(_ && _)
                then gotoFirstChild(isOk)
                else gotoSibling(isOk = false)

          case Node.Embed(_) =>
            gotoSibling(isOk)

    impl(root, 0, true)
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

  final case class Atom(
      isLookup: Boolean = false,
      isLookdown: Boolean = false,
      hasScope: Boolean = false,
      showSource: Boolean = false
  ) extends Shape

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
  case object errorMsg extends TokenObj(Atom(showSource = true))
  case object errorAST extends TokenObj(AnyShape)

  case object lift extends TokenObj(Fields(liftDest, liftNode, origNode)):
    def apply(dest: Token, node: Node, nodeInPlace: Node): Node =
      lift(
        liftDest(dest()),
        liftNode(node),
        origNode(nodeInPlace)
      )
  end lift
  case object liftDest extends TokenObj(AnyShape)
  case object liftNode extends TokenObj(AnyShape)
  case object origNode extends TokenObj(AnyShape)
end Builtin
