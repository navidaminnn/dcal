package distcompiler

import scala.collection.mutable
import distcompiler.Wellformed.detail.TokenAccessor

trait Token(using containingCtx: Wellformed)(
    shape: Wellformed.Shape
) extends Equals:
  self: Singleton & Product =>

  final val ctx: Wellformed = containingCtx
  final val idx: Int = ctx.nextIdx()

  ctx.addToken(productPrefix, this)
  Wellformed.`::=`(this)(using ctx)(shape)

  final override def canEqual(that: Any): Boolean =
    that.isInstanceOf[Token]

  final override def equals(that: Any): Boolean =
    that match
      case that: Token =>
        (ctx eq that.ctx)
        && idx == that.idx
      case _ => false

  final override def hashCode(): Int =
    (ctx, idx).hashCode()

  final def name: String =
    s"${ctx.tokenPrefix}.$productPrefix"
end Token

object Token:
  import Wellformed.*

  extension (token: Token)
    def apply(handles: Node.Handle*): Node =
      Node(token, mutable.ArrayBuffer.from(handles))

    def toAccessor: Wellformed.TokenAccessor = token

  extension (token: => Token)
    def defer: Wellformed.TokenAccessor = TokenAccessor(() => token)
end Token

trait Wellformed:
  import Wellformed.*

  def tokenPrefix: String
  val outerCtxOpt: Option[Wellformed]

  private val shapeMap = mutable.Map.empty[Token, Shape]
  private val nameMap = mutable.Map.empty[String, Token]

  final def addToken(name: String, token: Token): Unit =
    require(!nameMap.contains(name))
    nameMap(name) = token

  object nextIdx {
    private var counter = 0

    def apply(): Int =
      val result = counter
      counter += 1
      result
  }

  protected final given ctx: Wellformed = this

  Builtin.top ::= AnyShape

  final def checkNode(root: Node): Boolean =
    ???
end Wellformed

object Wellformed:
  object detail:
    opaque type TokenAccessor = (() => Token) | Token

    object TokenAccessor:
      given fromToken: Conversion[Token, TokenAccessor] = identity

      def apply(fn: () => Token): TokenAccessor = fn

      extension (accessor: TokenAccessor)
        def token: Token =
          accessor match
            case token: Token                        => token
            case tokenFn: (() => (Token @unchecked)) => tokenFn()
    end TokenAccessor
  end detail

  def basedOn(using self: Wellformed)(
      other: Wellformed
  ): Unit =
    self.shapeMap.addAll(other.shapeMap)

  extension (token: Token)(using ctx: Wellformed)
    @scala.annotation.alpha("assignShape")
    def ::=(shape: Shape): Unit =
      ctx.shapeMap(token) = shape

    def modifyShape(fn: Shape => Shape): Unit =
      ctx.shapeMap.updateWith(token): shapeOpt =>
        require(shapeOpt.nonEmpty)
        Some(fn(shapeOpt.get))

  export detail.TokenAccessor

  sealed trait Shape

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

  final class Choice(choiceSeq: TokenAccessor*) extends Shape:
    lazy val choices: Set[Token] =
      choiceSeq.iterator
        .map(_.token)
        .toSet

    @scala.annotation.alpha("or")
    def |(other: TokenAccessor): Choice =
      Choice((choiceSeq :+ other)*)

    def extend(choices: Token*): Choice =
      Choice((this.choices ++ choices).iterator.map(_.toAccessor).toSeq*)

    def restrict(choices: Token*): Choice =
      Choice((this.choices -- choices).iterator.map(_.toAccessor).toSeq*)
  end Choice

  object Choice:
    given tokenAsChoice: Conversion[Token, Choice] with
      def apply(token: Token): Choice =
        Choice(token)
  end Choice

  final case class Repeated(choice: Choice) extends Shape
end Wellformed

trait WellformedObj extends Wellformed:
  self: Singleton & Product =>

  final override def tokenPrefix: String =
    productPrefix

  final override val outerCtxOpt: Option[Wellformed] = None
end WellformedObj

trait NestedWellformed(using outerCtx: Wellformed) extends Wellformed:
  self: Singleton & Product =>

  final override def tokenPrefix: String =
    s"${outerCtx.tokenPrefix}.$productPrefix"

  final override val outerCtxOpt: Option[Wellformed] = Some(outerCtx)
end NestedWellformed

case object Builtin extends WellformedObj {
  import Wellformed.*

  case object top extends Token(AnyShape)
  case object error extends Token(Fields(errorMsg, errorAST)):
    def apply(msg: String, ast: Node): Node =
      error(
        errorMsg().at(Source(msg)),
        errorAST(ast)
      )
  end error
  case object errorMsg extends Token(Atom(showSource = true))
  case object errorAST extends Token(AnyShape)

  case object lift extends Token(Fields(liftDest, liftNode, origNode)):
    def apply(dest: Token, node: Node, nodeInPlace: Node): Node =
      lift(
        liftDest(dest()),
        liftNode(node),
        origNode(nodeInPlace)
      )
  end lift
  case object liftDest extends Token(AnyShape)
  case object liftNode extends Token(AnyShape)
  case object origNode extends Token(AnyShape)
}
