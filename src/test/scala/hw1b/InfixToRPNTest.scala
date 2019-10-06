package hw1b

import org.scalatest.FunSuite

import Main.Lexeme
import Main.Operator
import Main.Operand
import Main.infixToRPN

class InfixToRPNTest extends FunSuite {

  test("2 + 2") {
    assert(infixToRPN(List(Operand(2), Operator("+"), Operand(2))) ===
                      List(Operand(2), Operand(2), Operator("+")))
  }

  test("3 - 4") {
    assert(infixToRPN(List(Operand(3), Operator("-"), Operand(4))) ===
                      List(Operand(3), Operand(4), Operator("-")))
  }

  test("2 + 3 - 4") {
    assert(infixToRPN(List(Operand(2), Operator("+"), Operand(3), Operator("-"), Operand(4))) ===
                      List(Operand(2), Operand(3), Operator("+"), Operand(4), Operator("-")))
  }
}
