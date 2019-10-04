package hw1b

import org.scalatest.FunSuite

import Main.convertFromRomanNum

class ConvertFromRomanNumTest extends FunSuite {

  test("I") {
    assert(convertFromRomanNum("I") === 1)
  }

  test("IV") {
    assert(convertFromRomanNum("IV") === 4)
  }

  test("Gibberish"){
    assertThrows[IllegalArgumentException]{convertFromRomanNum("VVV")}
  }

}
