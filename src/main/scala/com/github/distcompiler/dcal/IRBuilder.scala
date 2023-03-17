package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.{Expression, Statement}
import com.github.distcompiler.dcal.DCalAST.Expression.*
import com.github.distcompiler.dcal.DCalAST.Statement.*
import com.github.distcompiler.dcal.DCalParser.*
import com.github.distcompiler.dcal.Utils.IRUtils

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
 * Compiles DCal concrete syntax to an IR that resembles TLA+. DCal statements that compile to TLA+ identifiers, let
 * expressions, and set maps are preserved by their respective IR Node structures. All other DCal statements are
 * uninterpreted by this IR pass.
 *
 * Each DCal statement compiles to a TLA+ nested let expression, not another TLA+ statement in a one-to-one mapping
 * manner.
 */
object IRBuilder {
  enum NameInfo {
    case Local
    case State
  }

  inline def ctx(using ctx: Context): Context = ctx

  final case class Context(stateName: String,
                           nameInfoOf: Map[String,NameInfo],
                           mapFilterOnSetInfo: (String, List[IR.Node]) = null,
                           rootsAndCounters: Map[String,Int] = Map.empty) {

    def withNameInfo(name: String, nameInfo: NameInfo): Context =
      copy(nameInfoOf = nameInfoOf.updated(name, nameInfo))

    def withMapFilterOnSetInfo(setMember: String, set: List[IR.Node]): Context =
      copy(mapFilterOnSetInfo = (setMember, set))

    private def withStateName(stateName: String): Context =
      copy(stateName = stateName)

    private def withFreshName[T](hint: String = "_name")(fn: Context ?=> String => T): T = {
      val (cleanName, newRootsAndCounters) = rootsAndCounters.get(hint) match {
        case None =>
          (s"$hint${1}", rootsAndCounters.updated(hint, 2))
        case Some(count) =>
          (s"$hint$count", rootsAndCounters.updated(hint, count + 1))
      }
      given Context = copy(rootsAndCounters = newRootsAndCounters)
      fn(cleanName)
    }

    def withFreshStateName[T](fn: Context ?=> String => T): T =
      withFreshName("_state") { stateName =>
        fn(using ctx.withStateName(stateName))(stateName)
      }

    def withFreshLocal[T](fn: Context ?=> String => T): T =
      withFreshName("l") { localName =>
        fn(using ctx.withNameInfo(localName, NameInfo.Local))(localName)
      }
  }

  private def generateBinOp(dcalBinOp: DCalAST.BinOp): IR.Node = {
    dcalBinOp match {
      case DCalAST.BinOp.Plus => IR.Node.Uninterpreted(" + ")
      case DCalAST.BinOp.Minus => IR.Node.Uninterpreted(" - ")
    }
  }

  private def generateRelOp(dcalRelOp: DCalAST.RelOp): IR.Node = {
    dcalRelOp match {
      case DCalAST.RelOp.LesserThan => IR.Node.Uninterpreted(" < ")
      case DCalAST.RelOp.LesserThanOrEqualTo => IR.Node.Uninterpreted(" <= ")
      case DCalAST.RelOp.GreaterThan => IR.Node.Uninterpreted(" > ")
      case DCalAST.RelOp.GreaterThanOrEqualTo => IR.Node.Uninterpreted(" >= ")
      case DCalAST.RelOp.EqualTo => IR.Node.Uninterpreted(" = ")
      case DCalAST.RelOp.NotEqualTo => IR.Node.Uninterpreted(" # ")
    }
  }

  private def generateLogicOp(dcalLogicOp: DCalAST.LogicOp): IR.Node = {
    dcalLogicOp match
      case DCalAST.LogicOp.And => IR.Node.Uninterpreted(" /\\ ")
      case DCalAST.LogicOp.Or => IR.Node.Uninterpreted(" \\/ ")
      //      case DCalAST.LogicOp.Not => IR.Node.Uninterpreted(" ~ ") TODO: Fix
  }

  private def generateExpression(dcalExpr: DCalAST.Expression)(using Context): List[IR.Node] = {
    dcalExpr match {
      case Expression.True => List(IR.Node.Uninterpreted("TRUE"))

      case Expression.False => List(IR.Node.Uninterpreted("FALSE"))

      case Expression.IntLiteral(value) => List(IR.Node.Uninterpreted(value.toString))

      case Expression.StringLiteral(value) => List(IR.Node.Uninterpreted(s""""$value""""))

      case Expression.Name(name) => ctx.nameInfoOf(name) match {
        case NameInfo.State => List(IR.Node.Name(ctx.mapFilterOnSetInfo._1), IR.Node.Uninterpreted(s".$name"))
        case NameInfo.Local => List(IR.Node.Name(name))
      }

      case Expression.ExpressionBinOp(lhs, binOp, rhs) =>
        generateExpression(lhs) ++ List(generateBinOp(binOp)) ++ generateExpression(rhs)

      case Expression.ExpressionRelOp(lhs, relOp, rhs) =>
        generateExpression(lhs) ++ List(generateRelOp(relOp)) ++ generateExpression(rhs)

      case ExpressionLogicOp(lhs, logicOp, rhs) =>
        generateExpression(lhs) ++ List(generateLogicOp(logicOp)) ++ generateExpression(rhs)

      case Expression.ExpressionUnOp(unop, expr) => ???

      case Expression.BracketedExpression(expr) =>
        List(IR.Node.Uninterpreted("(")) ++ generateExpression(expr) ++ List(IR.Node.Uninterpreted(")"))

      case Set(members) =>
        IR.Node.Uninterpreted("{ ")::IRUtils.delimit(
          members.map(generateExpression), IR.Node.Uninterpreted(", ")
        ) ++ List(IR.Node.Uninterpreted(" }"))
    }
  }

  private def generateAwait(dcalAwait: DCalAST.Statement.Await)(using Context): List[IR.Node] = {
    ctx.withFreshLocal( newLocalName =>
      List(
        IR.Node.FilterOnSet(
          set = List(IR.Node.Name(ctx.stateName)),
          setMember = newLocalName,
          pred = generateExpression(dcalAwait.expression)(using ctx.withMapFilterOnSetInfo(newLocalName, List(IR.Node.Name(ctx.stateName))))
        )
      )
    )
  }

  /**
   * Examples:
   * Assume str is a state, _state1 is the current state
   *    str := "new string" -> { [s EXCEPT !.str = "new string"]: s \in _state1 }
   * Assume y & i are states, v is a local, _state1 is the current state
   *    y := y - v || i := i + 1 -> { [s EXCEPT !.y = s.y - v, !.i = s.i + 1 ]: s \in _state1 }
   */
  private def generateAssignPairs(dcalAssignPairs: DCalAST.Statement.AssignPairs)(using Context): List[IR.Node] = {
    /**
     * Examples: Assume str, y, i are all state variables
     * str := "new string"  -> !.str = "new string"
     * y := y - v           -> !.y = s.y - v
     * i := i + 1           -> !.i = s.i + 1
     */
    def generateAssignPair(dcalAssignPair: DCalAST.AssignPair)(using Context): List[IR.Node] =
      ctx.nameInfoOf(dcalAssignPair.name) match {
        case NameInfo.Local => ??? // Should be unreachable because only var (which is a state variable) is mutable
        case NameInfo.State =>
          List[IR.Node](IR.Node.Uninterpreted(s"!.${dcalAssignPair.name} = ")) ++
            generateExpression(dcalAssignPair.expression)
      }

    def generateProc(using Context): List[IR.Node] = {
      val pb = ListBuffer[IR.Node](
        IR.Node.Uninterpreted("["),
        IR.Node.Name(ctx.mapFilterOnSetInfo._1),
        IR.Node.Uninterpreted(" EXCEPT "),
      )
      pb.appendAll(
        IRUtils.delimit(
          dcalAssignPairs.assignPairs.map(generateAssignPair), IR.Node.Uninterpreted(", ")
        )
      )
      pb.append(IR.Node.Uninterpreted("]"))
      pb.toList
    }

    ctx.withFreshLocal( newLocalName =>
      List(
        IR.Node.MapOnSet(
          set = List(IR.Node.Name(ctx.stateName)),
          setMember = newLocalName,
          proc = generateProc(using ctx.withMapFilterOnSetInfo(newLocalName, List(IR.Node.Name(ctx.stateName))))
        )
      )
    )

  }

  /**
   * Examples:
   * _state1 is the current state
   *    if x <= y then { x := x + 1 } else { y := y - 1 } ->
   *    UNION { IF s.x <= s.y
   *            THEN LET _state3 == { [s EXCEPT !.x = s.x + 1]: ss \in { s } } IN _state3
   *            ELSE LET _state3 == { [s EXCEPT !.y = s.y - 1]: ss \in { s } } IN _state3
   *          : s \in _state1 }
   */
  private def generateIfThenElse(dcalIfThenElse: DCalAST.Statement.IfThenElse)
                                (using Context): List[IR.Node]
  = {
    ctx.withFreshLocal( newLocalName =>
      def generateProc(using ctx: Context): List[IR.Node] = {
        val pb = ListBuffer[IR.Node](IR.Node.Uninterpreted("IF "))
        val predicate = generateExpression(dcalIfThenElse.predicate)
        pb.appendAll(predicate)

        ctx.withFreshStateName(newStateName =>
          val thenBlock = IR.Node.Let(
            name = newStateName,
            binding = List(IR.Node.Uninterpreted("{ "), IR.Node.Name(ctx.mapFilterOnSetInfo._1), IR.Node.Uninterpreted(" }")),
            body = generateStatements(dcalIfThenElse.thenBlock.statements)
          )
          pb.append(IR.Node.Uninterpreted("\nTHEN "))
          pb.append(thenBlock)
        )

        ctx.withFreshStateName(newStateName =>
          val elseBlock = IR.Node.Let(
            name = newStateName,
            binding = List(IR.Node.Uninterpreted("{ "), IR.Node.Name(ctx.mapFilterOnSetInfo._1), IR.Node.Uninterpreted(" }")),
            body = generateStatements(dcalIfThenElse.elseBlock.statements)
          )
          pb.append(IR.Node.Uninterpreted("\nELSE "))
          pb.append(elseBlock)
        )

        pb.toList
      }

      List(
        IR.Node.Uninterpreted("UNION "),
        IR.Node.MapOnSet(
          set = List(IR.Node.Name(ctx.stateName)),
          setMember = newLocalName,
          proc = generateProc(using ctx.withMapFilterOnSetInfo(newLocalName, List(IR.Node.Name(ctx.stateName))))
        )
      )
    )
  }

  private def generateLet(dcalLet: DCalAST.Statement.Let, rest: List[DCalAST.Statement])(using Context): List[IR.Node] = {
    dcalLet.assignmentOp match
      case DCalAST.AssignmentOp.EqualTo =>
        ctx.withFreshLocal( newLocalName =>
          List(
            IR.Node.Uninterpreted("UNION"),
            IR.Node.MapOnSet(
              set = List(IR.Node.Name(ctx.stateName)),
              setMember = newLocalName,
              proc = ctx
                .withMapFilterOnSetInfo(newLocalName, List(IR.Node.Name(ctx.stateName)))
                .withFreshStateName(newStateName =>
                  List(
                    IR.Node.Let(
                      name = dcalLet.name,
                      binding = dcalLet.binding match {
                        case Left(value) => ???
                        case Right(valueBinding) => generateExpression(valueBinding)
                      },
                      body = List(
                        IR.Node.Let(
                          name = newStateName,
                          binding = List(
                            IR.Node.Uninterpreted("{ "),
                            IR.Node.Name(newLocalName),
                            IR.Node.Uninterpreted(" }")
                          ),
                          body = generateStatements(rest)(using ctx.withNameInfo(dcalLet.name, NameInfo.Local))
                        )
                      )
                    )
                  )
                )
            )
          )
        )

      case DCalAST.AssignmentOp.SlashIn =>
        ctx.withFreshLocal( newLocalName =>
          def generateInnerProc(using Context): List[IR.Node] =
            ctx.withFreshStateName(newStateName =>
              List(
                IR.Node.Let(
                  name = newStateName,
                  binding = List(
                    IR.Node.Uninterpreted("{ "),
                    IR.Node.Name(newLocalName),
                    IR.Node.Uninterpreted(" }")
                  ),
                  body = generateStatements(rest)
                )
              )
            )

          def generateOuterProc(using Context): List[IR.Node] =
            val innerSetMember = dcalLet.name
            val set = dcalLet.binding match {
              case Left(value) => ???
              case Right(valueBinding) => generateExpression(valueBinding)
            }
            List(
              IR.Node.Uninterpreted("UNION "),
              IR.Node.MapOnSet(
                set = set,
                setMember = innerSetMember,
                proc = generateInnerProc(using ctx.withNameInfo(innerSetMember, NameInfo.Local))
              )
            )

          List(
            IR.Node.Uninterpreted("UNION "),
            IR.Node.MapOnSet(
              set = List(IR.Node.Name(ctx.stateName)),
              setMember = newLocalName,
              proc = generateOuterProc(using ctx
                .withMapFilterOnSetInfo(newLocalName, List(IR.Node.Name(ctx.stateName)))
              )
            )
          )
        )
  }

  private def generateVar(dcalVar: DCalAST.Statement.Var)(using Context): List[IR.Node] = {
    val name = dcalVar.name
    val assignmentOp = dcalVar.expressionOpt.get._1
    // TODO: If expressionOpt matches None, set var to TLA+ constant NULL value, where NULL is set to a model value.
    //  Not sure how to do this yet after several tries at changing the config file.
    val expr = dcalVar.expressionOpt.get._2
    assignmentOp match {
      case DCalAST.AssignmentOp.EqualTo =>
        ctx.withFreshLocal( setMember =>
          ctx.withFreshLocal( setKey =>
            List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name(ctx.stateName)),
                setMember = setMember,
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Uninterpreted(s"""$setKey \\in DOMAIN """),
                  IR.Node.Name(setMember),
                  IR.Node.Uninterpreted(s""" \\cup { "$name" } |-> IF $setKey = "$name" THEN """),
                ) ++
                  generateExpression(expr) ++
                  List(
                    IR.Node.Uninterpreted(" ELSE "),
                    IR.Node.Name(setMember),
                    IR.Node.Uninterpreted(s"[$setKey]]"),
                  )
              )
            )
          )
        )

      case DCalAST.AssignmentOp.SlashIn => ??? // Should be unreachable
    }

  }

  /**
   * Recursively generates statements.
   * In the base case, when there is no DCal statement left to generate, produces the current state.
   * In the recursive case, when there is at least a DCal statement to generate, produces a LET expression whose:
   * - name is a new state,
   * - binding is a set of states produced by mapping the DCal statement to the current state,
   * - body is an expression produced by the generating the remaining DCal statements.
   */
  private def generateStatements(dcalStmts: List[DCalAST.Statement])
                                (using Context): List[IR.Node] = {
    dcalStmts match {
      case Nil => List(IR.Node.Name(ctx.stateName))
      case Var(name, Some((DCalAST.AssignmentOp.SlashIn, expr)))::ss =>
        ctx.withFreshLocal(newLocalName =>
          generateStatements(
            Let(
              newLocalName, DCalAST.AssignmentOp.SlashIn, Right(expr)
            ) :: Var(
              name, Some((DCalAST.AssignmentOp.EqualTo, DCalAST.Expression.Name(newLocalName)))
            ) :: ss
          )
        )
      case s::ss =>
        val sInTla: List[IR.Node] =
          s match {
            case await @ Await(_) =>
              generateAwait(await)
            case assignPairs @ AssignPairs(_) =>
              generateAssignPairs(assignPairs)
            case let @ Let(_, _, _) =>
              generateLet(let, ss)
            case `var` @ Var(_, _) =>
              generateVar(`var`)
            case ifThenElse @ IfThenElse(_, _, _) =>
              generateIfThenElse(ifThenElse)
            case Statement.Call(_) => ???
          }
        s match
          case Let(_, _, _) => sInTla
          case Var(name, _) =>
            ctx.withFreshStateName( newStateName =>
              List(
                IR.Node.Let(
                  name = newStateName,
                  binding = sInTla,
                  body = generateStatements(ss)(using ctx.withNameInfo(name, NameInfo.State))
                )
              )
            )
          case _ =>
            ctx.withFreshStateName( newStateName =>
              List(
                IR.Node.Let(
                  name = newStateName,
                  binding = sInTla,
                  body = generateStatements(ss)
                )
              )
            )
    }
  }

  private def generateDefinition(dcalDef: DCalAST.Definition): IR.Definition = {
    var initialCtx = Context(
      nameInfoOf = Map[String, NameInfo](
        "str" -> NameInfo.State,
        "x" -> NameInfo.State,
        "y" -> NameInfo.State,
        "i" -> NameInfo.State,
        "set" -> NameInfo.State
      ),
      stateName = "_state1",
      rootsAndCounters = Map[String, Int]("_state" -> 2)
    )

    dcalDef.params.foreach(
      param => initialCtx = initialCtx.withNameInfo(param, NameInfo.Local)
    )

    IR.Definition(
      name = dcalDef.name,
      params = initialCtx.stateName +: dcalDef.params,
      body = generateStatements(dcalDef.body.statements)(using initialCtx)
    )
  }

  private def generateDefinition(dcalImport: String): IR.Definition = ???

  private def build(dcalModule: DCalAST.Module): IR.Module = {
    val definitions = dcalModule.definitions.map(generateDefinition)
    val imports = dcalModule.imports.map(generateDefinition)
    IR.Module(
      name = dcalModule.name,
      definitions = imports ++ definitions,
    )
  }

  def apply(contents: String, fileName: String): IR.Module = {
    val dcalModule = DCalParser(contents = contents, fileName = fileName)
    build(dcalModule = dcalModule)
  }
}
