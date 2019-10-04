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

  test("Find primes 55") {
    assert(findPrimes(55) === Set(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53))
  }

}
