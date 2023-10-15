package distcompiler.dcal

import scala.compiletime.asMatchable

import cats.*
import cats.data.*
// import cats.derived.*
import cats.syntax.all.given

import distcompiler.parsing.{Ps, SourceLocation, SourceLocated}
import distcompiler.transform.Poly
// import distcompiler.util.!!!

final case class Scoped[T](value: Ps[T], rootPath: Scoped.RootPath) extends SourceLocated {
  export value.sourceLocation

  def combineSourceLocation(sourceLocation: SourceLocation): Scoped[T] =
    copy(value.combineSourceLocation(sourceLocation))

  def extract: T = value.extract

  def refName(name: String): Scoped.NameRef =
    (rootPath, name)
  def refThisName(using ev: T <:< String): Scoped.NameRef =
    (rootPath, ev(extract))
  def exprRef(using ev: T <:< Scoped.AST.NExpression): Scoped.ExprRef =
    (rootPath, IdAST.stripFrom(Scoped.AST)(copy(value = value.as(ev(extract)))).asInstanceOf[IdAST.Expression])
}

object Scoped {
  object AST extends ASTF[Scoped]

  type RootPath = List[IdAST.ScopeRoot]
  
  type NameRef = (Scoped.RootPath, String)
  // object NameRef {
  //   extension (self: NameRef)
  // }

  type ExprRef = (Scoped.RootPath, IdAST.Expression)

  def scopeExpr[F[+_] <: SourceLocated : Comonad](src: ASTF[F])(expr: src.Expression): AST.Expression = {
    object impl extends Poly, Poly.Identity, Poly.GenericTransform, Poly.FunctorTransform {
      var rootPath: RootPath = Nil

      given scopeF[T, U](using subCase: =>Case[T, U]): GeneralCase[F[T], Scoped[U]] with {
        def apply(t: F[T]): Scoped[U] =
          Scoped(Ps(subCase(t.extract))(using t.sourceLocation), rootPath)
      }

      // resolve ambiguity between NEL and product deconstruction cases
      given scopeNonEmptyList[T, U](using subCase: =>Case[T, U]): GeneralCase[NonEmptyList[T], NonEmptyList[U]] =
        functorTransformCase

      given scopeNode[T <: src.NNode, U <: AST.NNode](using generalCase: =>GeneralCase[F[T], Scoped[U]]): SpecialCase[F[T], Scoped[U]] = new SpecialCase {
        def apply(srcNode: F[T]): Scoped[U] = {
          val maybeRoot = srcNode.extract
          // for some reason Scala doesn't like me type testing NScopeRoot (it claims all cases satisfy that, which makes no sense), so I do it with a boolean instead :shrug:
          if(maybeRoot.isScopeRoot) {
            rootPath = IdAST.stripFrom(src)(srcNode).asInstanceOf[IdAST.ScopeRoot] :: rootPath
            val result = generalCase(srcNode)
            rootPath = rootPath.tail
            result
          } else {
            generalCase(srcNode)
          }
        }
      }

    }

    impl(expr)
  }

  given comonad: Comonad[Scoped] with {
    def extract[A](x: Scoped[A]): A =
      x.extract
    
    def coflatMap[A, B](fa: Scoped[A])(f: Scoped[A] => B): Scoped[B] =
      fa.coflatMap(f)

    def map[A, B](fa: Scoped[A])(f: A => B): Scoped[B] =
      coflatMap(fa)(fa => f(extract(fa)))
  }
}
