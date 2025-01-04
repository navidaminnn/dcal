package distcompiler.calc

import cats.syntax.all.given

import distcompiler.*
import distcompiler.Builtin.{Error, SourceMarker}
import Reader.*

object CalcReader extends Reader:
  import distcompiler.dsl.*
  import distcompiler.Builtin.{Error, SourceMarker}
  import Reader.*

  def wellformed: Wellformed = distcompiler.calc.wellformed

  private val digit: Set[Char] = ('0' to '9').toSet
  private val operation: Set[Char] = Set('*', '/', '+', '-')
  private val whitespace: Set[Char] = Set(' ', '\n', '\t')

  private lazy val unexpectedEOF: Manip[SourceRange] =
    consumeMatch: m =>
      addChild(Error("unexpected EOF", SourceMarker(m)))
        *> Manip.pure(m)

  protected lazy val rules: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(whitespace):
          extendThisNodeWithMatch(rules)
        .onOneOf(digit):
          numberMode
        .onOneOf(operation):
          operationMode
        .fallback:
          bytes.selectCount(1):
            consumeMatch: m =>
              addChild(Error("invalid byte", SourceMarker(m)))
                *> rules
          | consumeMatch: m =>
            on(theTop).check
              *> Manip.pure(m)
          | unexpectedEOF

  private lazy val numberMode: Manip[SourceRange] =
    commit:
      bytes
        .selecting[SourceRange]
        .onOneOf(digit)(numberMode)
        .fallback:
          consumeMatch: m =>
            m.decodeString().toIntOption match
              case Some(value) =>
                addChild(tokens.Atom(m))
                  *> rules
              case None =>
                addChild(Error("invalid number format", SourceMarker(m)))
                  *> rules

  private lazy val operationMode: Manip[SourceRange] =
    commit:
      consumeMatch: m =>
        addChild(tokens.Atom(m))
          *> rules

  def evaluate(top: Node.Top): Int =
    val tokens = top.children.iterator.map(n => 
      n.asNode.sourceRange.decodeString().trim
    ).toList

    def validateTokens(tokens: List[String]): Unit =
      if tokens.isEmpty then 
        throw IllegalArgumentException("Empty expression")
      
      if !tokens.head.forall(_.isDigit) then 
        throw IllegalArgumentException("Expression must start with a number")
      
      if !tokens.last.forall(_.isDigit) then 
        throw IllegalArgumentException("Expression must end with a number")
      
      val isValid = tokens.zipWithIndex.forall { case (token, i) =>
        if i % 2 == 0 then 
          token.forall(_.isDigit)
        else 
          Set("+", "-", "*", "/").contains(token)
      }
      
      if !isValid then 
        throw IllegalArgumentException("Invalid expression: must alternate between numbers and operators")

    def evalOp(left: Int, op: String, right: Int): Int = op match
      case "*" => left * right
      case "/" => 
        if right == 0 then throw new ArithmeticException("Division by zero")
        left / right
      case "+" => left + right
      case "-" => left - right
      case _ => throw new IllegalArgumentException(s"Invalid operator: $op")

    def evaluateWithPrecedence(tokens: List[String]): Int =
      def evalMulDiv(tokens: List[String]): List[String] =
        def isNumber(s: String) = s.forall(_.isDigit)
        
        tokens match
          case num1 :: "*" :: num2 :: rest if isNumber(num1) && isNumber(num2) =>
            val result = evalOp(num1.toInt, "*", num2.toInt)
            evalMulDiv(result.toString :: rest)
          
          case num1 :: "/" :: num2 :: rest if isNumber(num1) && isNumber(num2) =>
            val result = evalOp(num1.toInt, "/", num2.toInt)
            evalMulDiv(result.toString :: rest)
          
          // if not multiplication or division, keep first token and process rest
          case firstToken :: remainingTokens => 
            firstToken :: evalMulDiv(remainingTokens)
          
          case Nil => Nil

      val afterMulDiv = evalMulDiv(tokens)
      afterMulDiv.sliding(3, 2).foldLeft(afterMulDiv.head.toInt) {
        case (acc, List(_, "+", right)) => acc + right.toInt
        case (acc, List(_, "-", right)) => acc - right.toInt
        case (acc, _) => acc
      }

    validateTokens(tokens)
    evaluateWithPrecedence(tokens)

  // def evaluate(top: Node.Top): Int =
  //   def evalNode(nodes: Iterator[Node.Child]): Int =
  //     if !nodes.hasNext then 0
  //     else
  //       var result = nodes.next().asNode.sourceRange.decodeString().trim.toInt
  //       while nodes.hasNext do
  //         val op = nodes.next().asNode.sourceRange.decodeString().trim.head
  //         if !nodes.hasNext then
  //           throw new IllegalArgumentException("Expression ends with an operator")
  //         val next = nodes.next().asNode.sourceRange.decodeString().trim.toInt
  //         op match
  //           case '+' => result = result + next
  //           case '-' => result = result - next
  //           case _ => throw new IllegalArgumentException("Invalid equation provided")
  //       result

  //   evalNode(top.children.iterator)