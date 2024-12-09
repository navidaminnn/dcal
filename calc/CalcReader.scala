package distcompiler.calc

import cats.syntax.all.given

import distcompiler.*
import distcompiler.Builtin.{Error, SourceMarker}
import Reader.*

object CalcReader:
  private val digit: Set[Char] = ('0' to '9').toSet
  private val operations: Set[Char] = Set('+', '-', '*', '/')
  private val whitespace: Set[Char] = Set(' ', '\n', '\t')

  sealed trait ParsedToken
  case class Num(value: Int) extends ParsedToken
  case class Operation(value: Char) extends ParsedToken

  def tokenize(input: String): List[ParsedToken] = {
    val (tokens, leftoverBuffer) = input.foldLeft(List.empty[ParsedToken] -> new StringBuilder) {
      case ((tokenList, numberBuffer), char) if digit.contains(char) =>
        (tokenList, numberBuffer.append(char))

      case ((tokenList, numberBuffer), char) if operations.contains(char) =>
        val tokensWithNumbers = if (numberBuffer.nonEmpty)
          tokenList :+ Num(numberBuffer.toString().toInt)
        else tokenList
        (tokensWithNumbers :+ Operation(char), new StringBuilder)

      case((tokenList, numberBuffer), char) if whitespace.contains(char) =>
        val tokensWithNumbers = if (numberBuffer.nonEmpty)
          tokenList :+ Num(numberBuffer.toString().toInt)
        else tokenList
        (tokensWithNumbers, new StringBuilder)

      case _ => throw IllegalArgumentException("Invalid character")
    }

    tokens ++ (if (leftoverBuffer.nonEmpty) List(Num(leftoverBuffer.toString().toInt)) else Nil)
  }

  def evaluate(tokens: List[ParsedToken]): Int = {
    def precedence(operator: Char): Int = operator match {
      case '*' | '/'  => 1
      case '+' | '-'  => 0
    }

    def applyOperation(a: Int, b: Int, op: Char): Int = op match {
      case '+' => a + b
      case '-' => a - b
      case '*' => a * b
      case '/' => if (b != 0) a / b else throw ArithmeticException("Division by zero")
    }

    val (values, ops) = (
      new scala.collection.mutable.Stack[Int](),
      new scala.collection.mutable.Stack[Char]()
    )

    tokens.foreach {
      case Num(value) => 
        values.push(value)
      
      case Operation(op) =>
        while (ops.nonEmpty && precedence(ops.top) >= precedence(op)) {
          val b = values.pop()
          val a = values.pop()
          values.push(applyOperation(a, b, ops.pop()))
        }
        ops.push(op)
    }

    while (ops.nonEmpty) {
      val b = values.pop()
      val a = values.pop()
      values.push(applyOperation(a, b, ops.pop()))
    }

    values.pop()
  }