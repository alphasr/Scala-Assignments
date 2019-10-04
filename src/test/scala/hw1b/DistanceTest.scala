package hw1b

import org.scalatest.FunSuite

import Main.levensteinDistance

class LevensteinDistanceTest extends FunSuite {

  test("ola ala") {
    assert(levensteinDistance("ola", "ala") === 1)
  }

  test("hello ehllo") {
    assert(levensteinDistance("hello", "ehllo") === 2)
  }

  test(" ehllo") {
    assert(levensteinDistance("sittin", "sitting") === 1)
  }

}
