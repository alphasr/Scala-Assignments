package hw1b

import scala.collection.immutable._
import scala.collection.{immutable, mutable}

object Main extends App {
    /* Exercise 1 */
    def levensteinDistance(a: String, b: String): Int = ???

    /* Exercise 2 */
    abstract class Lexeme
    case class Operator(symbol: String) extends Lexeme
    case class Operand(value: Int) extends Lexeme

    def infixToRPN(expression: List[Lexeme]): List[Lexeme] = ???

    /* Exercise 3 */
    def findPrimes(n: Int): Set[Int] = {

        if(n==1){var s : Set[Int] = Set()
        return s}

        val primes = mutable.ArrayBuffer[Int](2)

        def isPrime(i: Int): Boolean =
            primes.takeWhile(_ <= math.sqrt(i).toInt).forall(i % _ != 0)

        for (k <- 3 until n)
            if (isPrime(k))
                primes += k
         primes.toSet
        val ss = collection.immutable.SortedSet[Int]() ++ primes

        ss
    }

    /* Exercise 4 */
    def convertFromRomanNum(n: String): Int = ???
  }
