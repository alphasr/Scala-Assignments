package hw1b

import org.scalatest.FunSuite

import Main.findPrimes

class FindPrimesTest extends FunSuite {

  test("Find primes 1") {
      assert(findPrimes(1) === Set())
  }

  test("Find primes 10") {
      assert(findPrimes(10) === Set(2,3,5,7))
  }


}
