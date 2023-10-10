package distcompiler.dcal

// import cats.*
// import cats.data.*
// import cats.derived.*
// //import cats.syntax.all.given

// import scala.collection.immutable.SortedMap
// import distcompiler.util.SummonTuple

// final class Typing[T](scoping: Scoping[T], val typingInfo: Typing.Info) {
//   // export scoping.scopingInfo

//   // def extract: T = scoping.extract

//   // def coflatMap[U](fn: Typing[T] => U): Typing[U] =
//   //   ???
// }

object Typing {
  // given Order[AST.PathSegment] = derivedSumOrder
  // given Order[AST.Param] = derivedSumOrder
  // given Order[AST.Definition.Qualifier] = derivedSumOrder
  // given Order[AST.Definition] = derivedSumOrder
  // given Order[AST.Statements] = derivedSumOrder
  // given Order[AST.CheckDirective] = derivedSumOrder
  // given Order[AST.InterfaceMember] = derivedSumOrder
  // given Order[AST.SingleStatement] = derivedSumOrder
  // given Order[AST.Binding] = derivedSumOrder
  // given Order[AST.ValBinding] = derivedSumOrder
  // given Order[AST.Expression] = derivedSumOrder
  // given Order[AST.AssignLhs] = derivedSumOrder
  // given Order[AST.CallArg] = derivedSumOrder
  // given Order[AST.QuantifierBound] = derivedSumOrder

  // enum TypeId derives Order {
  //   case Def(root: Scoping.RootPath, defn: AST.Definition.Def)
  //   case Var(root: Scoping.RootPath, varDef: AST.Statements.Var)
  //   case Let(root: Scoping.RootPath, letDef: AST.Statements.Let)
  //   case Impl(root: Scoping.RootPath, iface: AST.Definition.Impl)
  //   case Expr(root: Scoping.RootPath, expr: AST.Expression)
  //   case Param(root: Scoping.RootPath, param: AST.Param)
  //   case Interface(root: Scoping.RootPath, iface: AST.Definition.Interface)
  // }

  // enum Type {
  //   case Interface(ofs: List[TypeId.Interface], fields: SortedMap[String, TypeId])
  //   case Impl(of: NonEmptyList[TypeId.Interface], fields: SortedMap[String, TypeId])
  //   case Instance(of: TypeId.Impl, fields: SortedMap[String, TypeId])
  //   case Def(qualifierOpt: Option[AST.Definition.Qualifier], isAbstract: Boolean, argTypes: List[TypeId], resultType: TypeId)
  //   case Projection(root: TypeId, name: String)
  //   case Union(left: TypeId, right: TypeId)
  //   case Implements(above: TypeId, below: TypeId)
  // }

  // object Type {
  //   given monoid: Semigroup[Type] with {
  //     def combine(x: Type, y: Type): Type = ???
  //   }
  // }

  // final case class Info(typeOf: SortedMap[TypeId, Type]) derives Monoid {

  // }

  // object Info {

  // }


  // given derivedProductOrder[T <: Product](using mirror: deriving.Mirror.ProductOf[T])(using elemOrders: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, Order]]): Order[T] with {
  //   private lazy val impl: Order[T] =
  //     elemOrders
  //       .value
  //       .productIterator
  //       .asInstanceOf[Iterator[Order[Any]]]
  //       .zipWithIndex
  //       .map {
  //         case (order, idx) =>
  //           Order.by[T, Any](_.productElement(idx))(using order)
  //       }
  //       .foldLeft(Order.allEqual[T])(Order.whenEqual)
      
  //   export impl.compare
  // }

  // def derivedSumOrder[T](using mirror: deriving.Mirror.SumOf[T])(using elemOrders: =>SummonTuple[Tuple.Map[mirror.MirroredElemTypes, Order]]): Order[T] =
  //   Order.whenEqual(Order.by[T, Int](mirror.ordinal), new Order[T] {
  //     def compare(x: T, y: T): Int =
  //       elemOrders
  //         .value
  //         .productElement(mirror.ordinal(x))
  //         .asInstanceOf[Order[T]]
  //         .compare(x, y)
  //   })
}
