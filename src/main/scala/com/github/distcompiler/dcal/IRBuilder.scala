package com.github.distcompiler.dcal

import com.github.distcompiler.dcal.DCalAST.{Expression, Statement}
import com.github.distcompiler.dcal.DCalAST.Expression.*
import com.github.distcompiler.dcal.DCalAST.Statement.*
import com.github.distcompiler.dcal.DCalParser.*
import com.github.distcompiler.dcal.Utils.IRUtils

/**
 * Compiles DCal to an IR that is mostly uninterpreted TLA+, except DCal statements that compile to TLA+ identifiers,
 * let expressions, and set maps/filters. These are preserved by their respective IR Node structures.
 * Each DCal statement compiles to a TLA+ nested let expression, not another TLA+ statement in a one-to-one mapping
 * manner.
 */
object IRBuilder {
  enum NameInfo {
    case Local
    case State
  }

  private inline def ctx(using ctx: Context): Context = ctx

  private final case class Context(stateName: String,
                                   nameInfoOf: Map[String,NameInfo],
                                   mapFilterOnSetInfo: (String, List[IR.Node]) = null,
                                   rootsAndCounters: Map[String,Int] = Map.empty) {

    def withNameInfo(name: String, nameInfo: NameInfo): Context =
      copy(nameInfoOf = nameInfoOf.updated(name, nameInfo))

    def withNameInfo[T](name: String, nameInfo: NameInfo)(fn: Context ?=> T): T =
      fn(using withNameInfo(name, nameInfo))

    def withMapFilterOnSetInfo(setMember: String, set: List[IR.Node]): Context =
      copy(mapFilterOnSetInfo = (setMember, set))

    def withMapFilterOnSetInfo[T](setMember: String, set: List[IR.Node])(fn: Context ?=> T): T =
      fn(using withMapFilterOnSetInfo(setMember, set))

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

  private def generateBinOp(dcalBinOp: DCalAST.BinOp): Iterator[IR.Node] = {
    val binOpIR = dcalBinOp match {
      case DCalAST.BinOp.Plus => IR.Node.Uninterpreted(" + ")
      case DCalAST.BinOp.Minus => IR.Node.Uninterpreted(" - ")
    }
    Iterator(binOpIR)
  }

  private def generateRelOp(dcalRelOp: DCalAST.RelOp): Iterator[IR.Node] = {
    val relOpIR = dcalRelOp match {
      case DCalAST.RelOp.LesserThan => IR.Node.Uninterpreted(" < ")
      case DCalAST.RelOp.LesserThanOrEqualTo => IR.Node.Uninterpreted(" <= ")
      case DCalAST.RelOp.GreaterThan => IR.Node.Uninterpreted(" > ")
      case DCalAST.RelOp.GreaterThanOrEqualTo => IR.Node.Uninterpreted(" >= ")
      case DCalAST.RelOp.EqualTo => IR.Node.Uninterpreted(" = ")
      case DCalAST.RelOp.NotEqualTo => IR.Node.Uninterpreted(" # ")
    }
    Iterator(relOpIR)
  }

  private def generateLogicOp(dcalLogicOp: DCalAST.LogicOp): Iterator[IR.Node] = {
    val logicOpIR = dcalLogicOp match {
      case DCalAST.LogicOp.And => IR.Node.Uninterpreted(" /\\ ")
      case DCalAST.LogicOp.Or => IR.Node.Uninterpreted(" \\/ ")
    }
    Iterator(logicOpIR)
  }

  private def generateUnOp(dcalUnOp: DCalAST.UnOp): Iterator[IR.Node] = {
    val unOpIR = dcalUnOp match
      case DCalAST.UnOp.Not => IR.Node.Uninterpreted(" ~")
    Iterator(unOpIR)
  }

  private def generateExpression(dcalExpr: DCalAST.Expression)(using Context): Iterator[IR.Node] = {
    dcalExpr match {
      case Expression.True => Iterator(IR.Node.Uninterpreted("TRUE"))

      case Expression.False => Iterator(IR.Node.Uninterpreted("FALSE"))

      case Expression.IntLiteral(value) => Iterator(IR.Node.Uninterpreted(value.toString))

      case Expression.StringLiteral(value) => Iterator(IR.Node.Uninterpreted(s""""$value""""))

      case Expression.Name(name) => ctx.nameInfoOf(name) match {
        case NameInfo.State => Iterator(IR.Node.Name(ctx.mapFilterOnSetInfo._1), IR.Node.Uninterpreted(s".$name"))
        case NameInfo.Local => Iterator(IR.Node.Name(name))
      }

      case Expression.ExpressionBinOp(lhs, binOp, rhs) =>
        generateExpression(lhs) ++ generateBinOp(binOp) ++ generateExpression(rhs)

      case Expression.ExpressionRelOp(lhs, relOp, rhs) =>
        generateExpression(lhs) ++ generateRelOp(relOp) ++ generateExpression(rhs)

      case ExpressionLogicOp(lhs, logicOp, rhs) =>
        generateExpression(lhs) ++ generateLogicOp(logicOp) ++ generateExpression(rhs)

      case Expression.ExpressionUnOp(unop, expr) => generateUnOp(unop) ++ generateExpression(expr)

      case Expression.BracketedExpression(expr) =>
        Iterator(IR.Node.Uninterpreted("(")) ++ generateExpression(expr) ++ Iterator(IR.Node.Uninterpreted(")"))

      case Set(members) => Iterator(IR.Node.Uninterpreted("{ ")) ++
        IRUtils.delimit(
          members.map(m => generateExpression(m).toList),
          IR.Node.Uninterpreted(", ")
        ).iterator ++
        Iterator(IR.Node.Uninterpreted(" }"))
    }
  }

  private def generateAwait(dcalAwait: DCalAST.Statement.Await)(using Context): List[IR.Node] = {
    ctx.withFreshLocal { newLocalName =>
      List(
        IR.Node.FilterOnSet(
          set = List(IR.Node.Name(ctx.stateName)),
          setMember = newLocalName,
          pred = ctx.withMapFilterOnSetInfo(newLocalName, List(IR.Node.Name(ctx.stateName))) {
            generateExpression(dcalAwait.expression).toList
          }
        )
      )
    }
  }

  /**
   * Examples:
   * Assume str is a state, _state1 is the current state
   *    str := "new string" -> { [s EXCEPT !.str = "new string"]: s \in _state1 }
   * Assume y & i are states, v is a local, _state1 is the current state
   *    y := y - v || i := i + 1 -> { [s EXCEPT !.y = s.y - v, !.i = s.i + 1 ]: s \in _state1 }
   */
  private def generateAssignPairs(dcalAssignPairs: DCalAST.Statement.AssignPairs)(using Context): List[IR.Node] = {
    def generateAssignPair(dcalAssignPair: DCalAST.AssignPair)(using Context): List[IR.Node] =
      ctx.nameInfoOf(dcalAssignPair.name) match {
        case NameInfo.Local => ??? // Should be unreachable because only var (which is a state variable) is mutable
        case NameInfo.State =>
          IR.Node.Uninterpreted(s"!.${dcalAssignPair.name} = ")::generateExpression(dcalAssignPair.expression).toList
      }

    ctx.withFreshLocal { newLocalName =>
      List(
        IR.Node.MapOnSet(
          set = List(IR.Node.Name(ctx.stateName)),
          setMember = newLocalName,
          proc = ctx.withMapFilterOnSetInfo(newLocalName, List(IR.Node.Name(ctx.stateName))) {
            IR.Node.Uninterpreted("[")::
            IR.Node.Name(ctx.mapFilterOnSetInfo._1)::
            IR.Node.Uninterpreted(" EXCEPT ")::
            IRUtils.delimit(
              dcalAssignPairs.assignPairs.map(generateAssignPair), IR.Node.Uninterpreted(", ")
            ):::List(
              IR.Node.Uninterpreted("]")
            )
          }
        )
      )
    }
  }

  /**
   * Examples:
   * Assume _state1 is the current state
   *    if x <= y then { x := x + 1 } else { y := y - 1 }
   *    ->
   *    UNION { IF s.x <= s.y
   *            THEN LET _state3 == { [s EXCEPT !.x = s.x + 1]: ss \in { s } } IN _state3
   *            ELSE LET _state3 == { [s EXCEPT !.y = s.y - 1]: ss \in { s } } IN _state3
   *          : s \in _state1 }
   */
  private def generateIfThenElse(dcalIfThenElse: DCalAST.Statement.IfThenElse)
                                (using Context): List[IR.Node]
  = {
    ctx.withFreshLocal { newLocalName =>
      List(
        IR.Node.Uninterpreted("UNION "),
        IR.Node.MapOnSet(
          set = List(IR.Node.Name(ctx.stateName)),
          setMember = newLocalName,
          proc = ctx.withMapFilterOnSetInfo(newLocalName, List(IR.Node.Name(ctx.stateName))) {
            val predicate = generateExpression(dcalIfThenElse.predicate)
            val thenBlock = ctx.withFreshStateName { newStateName =>
              Iterator[IR.Node](
                IR.Node.Uninterpreted("\nTHEN "),
                IR.Node.Let(
                  name = newStateName,
                  binding = List(
                    IR.Node.Uninterpreted("{ "),
                    IR.Node.Name(ctx.mapFilterOnSetInfo._1),
                    IR.Node.Uninterpreted(" }")
                  ),
                  body = generateStatements(dcalIfThenElse.thenBlock.statements)
                )
              )
            }
            val elseBlock = ctx.withFreshStateName { newStateName =>
              Iterator[IR.Node](
                IR.Node.Uninterpreted("\nELSE "),
                IR.Node.Let(
                  name = newStateName,
                  binding = List(
                    IR.Node.Uninterpreted("{ "),
                    IR.Node.Name(ctx.mapFilterOnSetInfo._1),
                    IR.Node.Uninterpreted(" }")
                  ),
                  body = generateStatements(dcalIfThenElse.elseBlock.statements)
                )
              )
            }
            IR.Node.Uninterpreted("IF ")::(predicate ++ thenBlock ++ elseBlock).toList
          }
        )
      )
    }
  }

  private def generateLet(dcalLet: DCalAST.Statement.Let, rest: List[DCalAST.Statement])(using Context): List[IR.Node] = {
    dcalLet.assignmentOp match {
      case DCalAST.AssignmentOp.EqualTo =>
        ctx.withFreshLocal { newLocalName =>
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
                        case Left(callBinding) => ???
                        case Right(exprBinding) => generateExpression(exprBinding).toList
                      },
                      body = List(
                        IR.Node.Let(
                          name = newStateName,
                          binding = List(
                            IR.Node.Uninterpreted("{ "),
                            IR.Node.Name(newLocalName),
                            IR.Node.Uninterpreted(" }")
                          ),
                          body = ctx.withNameInfo(dcalLet.name, NameInfo.Local) {
                            generateStatements(rest)
                          }
                        )
                      )
                    )
                  )
                )
            )
          )
        }

      case DCalAST.AssignmentOp.SlashIn =>
        ctx.withFreshLocal { newLocalName =>
          List(
            IR.Node.Uninterpreted("UNION "),
            IR.Node.MapOnSet(
              set = List(IR.Node.Name(ctx.stateName)),
              setMember = newLocalName,
              proc = ctx.withMapFilterOnSetInfo(newLocalName, List(IR.Node.Name(ctx.stateName))) {
                val set = dcalLet.binding match {
                  case Left(callBinding) => ???
                  case Right(exprBinding) => generateExpression(exprBinding).toList
                }
                List(
                  IR.Node.Uninterpreted("UNION "),
                  IR.Node.MapOnSet(
                    set = set,
                    setMember = dcalLet.name,
                    proc = ctx.withNameInfo(dcalLet.name, NameInfo.Local) {
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
                    }
                  )
                )
              }
            )
          )
        }
    }
  }

  private def generateVar(dcalVar: DCalAST.Statement.Var)(using Context): List[IR.Node] = {
    val name = dcalVar.name
    val assignmentOp = dcalVar.expressionOpt.get._1
    // TODO: If expressionOpt matches None, set var to TLA+ constant NULL value, where NULL is set to a model value.
    //  Not sure how to do this yet after several tries at changing the config file.
    val binding = dcalVar.expressionOpt.get._2

    assignmentOp match {
      case DCalAST.AssignmentOp.EqualTo =>
        ctx.withFreshLocal { setMember =>
          ctx.withFreshLocal { setKey =>
            List(
              IR.Node.MapOnSet(
                set = List(IR.Node.Name(ctx.stateName)),
                setMember = setMember,
                proc = List(
                  IR.Node.Uninterpreted("["),
                  IR.Node.Uninterpreted(s"""$setKey \\in DOMAIN """),
                  IR.Node.Name(setMember),
                  IR.Node.Uninterpreted(s""" \\cup { "$name" } |-> IF $setKey = "$name" THEN """)
                ) ++
                  generateExpression(binding) ++
                  List(
                    IR.Node.Uninterpreted(" ELSE "),
                    IR.Node.Name(setMember),
                    IR.Node.Uninterpreted(s"[$setKey]]"),
                  )
              )
            )
          }
        }

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
  private def generateStatements(dcalStmts: List[DCalAST.Statement])(using Context): List[IR.Node] = {
    dcalStmts match {
      case Nil => List(IR.Node.Name(ctx.stateName))
      case Var(name, Some((DCalAST.AssignmentOp.SlashIn, expr)))::ss =>
        ctx.withFreshLocal(newLocalName =>
          generateStatements(
            Let(newLocalName, DCalAST.AssignmentOp.SlashIn, Right(expr)
            )::Var(
              name, Some((DCalAST.AssignmentOp.EqualTo, DCalAST.Expression.Name(newLocalName)))
            )::ss
          )
        )
      case s::ss =>
        val sInTla: List[IR.Node] =
          s match {
            case await @ Await(_) => generateAwait(await)
            case assignPairs @ AssignPairs(_) => generateAssignPairs(assignPairs).toList
            case let @ Let(_, _, _) => generateLet(let, ss)
            case `var` @ Var(_, _) => generateVar(`var`)
            case ifThenElse @ IfThenElse(_, _, _) => generateIfThenElse(ifThenElse)
            case Statement.Call(_) => ???
          }
        s match
          case Let(_, _, _) => sInTla
          case Var(name, _) => ctx.withFreshStateName { newStateName =>
            List(
              IR.Node.Let(
                name = newStateName,
                binding = sInTla,
                body = ctx.withNameInfo(name, NameInfo.State) {
                  generateStatements(ss)
                }
              )
            )
          }
          case _ => ctx.withFreshStateName { newStateName =>
            List(
              IR.Node.Let(
                name = newStateName,
                binding = sInTla,
                body = generateStatements(ss)
              )
            )
          }
    }
  }

  private def generateDefinition(dcalDef: DCalAST.Definition): IR.Definition = {
    var initialCtx = Context(
      // FIXME: These initial variables are added for testing. Remove them & rework tests.
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

    dcalDef.params.foreach{ param => initialCtx = initialCtx.withNameInfo(param, NameInfo.Local) }

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
    IR.Module(name = dcalModule.name, definitions = imports ++ definitions)
  }

  def apply(contents: String, fileName: String): IR.Module = {
    val dcalModule = DCalParser(contents = contents, fileName = fileName)
    build(dcalModule = dcalModule)
  }
}
