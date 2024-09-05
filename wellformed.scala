package distcompiler

import scala.collection.mutable
import scala.collection.MapView

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
    def apply(handles: Node.Handle*): Node =
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

  final def isBuilding: Boolean =
    deferredOps ne null

  final def deferBuildStep(step: => Unit): Unit =
    require(isBuilding)
    deferredOps.nn.addOne(() => step)

  final lazy val shapes: MapView[Token, Shape] =
    assert(isBuilding)
    deferredOps.nn.foreach(_.apply())
    deferredOps = null

    shapeMapBuilder.view

  final def checkNode(root: Node): Boolean =
    ???
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

  final class Choice(choices: Set[Token]) extends Shape:
    @scala.annotation.alpha("or")
    def |(other: Token): Choice =
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
  
  case object error extends TokenObj(Fields(errorMsg, errorAST)):
    def apply(msg: String, ast: Node): Node =
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
