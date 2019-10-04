package hw1b

import scala.collection.immutable._
import scala.collection.{immutable, mutable}
import scala.math.min

object Main extends App {
    /* Exercise 1 */
    def levensteinDistance(a: String, b: String): Int = {

      val lenStr1 = a.length
      val lenStr2 = b.length

      def min(nums: Int*): Int = nums.min

      val d: Array[Array[Int]] = Array.ofDim(lenStr1 + 1, lenStr2 + 1)

      for (i <- 0 to lenStr1) d(i)(0) = i
      for (j <- 0 to lenStr2) d(0)(j) = j

      for (i <- 1 to lenStr1; j <- 1 to lenStr2) {
        val cost = if (a(i - 1) == b(j - 1)) 0 else 1

        d(i)(j) = min(
          d(i-1)(j  ) + 1,
          d(i  )(j-1) + 1,
          d(i-1)(j-1) + cost
        )
      }

      d(lenStr1)(lenStr2)

    }


  /* Exercise 2 */
    abstract class Lexeme
    case class Operator(symbol: String) extends Lexeme{
      val str: String = symbol
    }
    case class Operand(value: Int) extends Lexeme{
      val num : Int = value
    }

    def infixToRPN(expression: List[Lexeme]): List[Lexeme] = {
      var dm  = List[Lexeme]()

      for(i <- 0 until dm.size) {
        if
      }

      dm
    }







    /* Exercise 3 */
    def findPrimes(n: Int): Set[Int] = {

      if (n == 1) {
        var s: Set[Int] = Set()
        return s
      }

      val primes = mutable.ArrayBuffer[Int](2)

      def isPrime(i: Int): Boolean =
        primes.takeWhile(_ <= math.sqrt(i).toInt).forall(i % _ != 0)

      for (k <- 3 until n) {
        if (isPrime(k))
          primes += k
      }

      primes.toSet

      val ss = collection.immutable.SortedSet[Int]() ++ primes

      ss
    }


    /* Exercise 4 */
    def convertFromRomanNum(n: String): Int = {
      val str = n.toUpperCase()
      var sum: Int = 0

      if(str.length ==1){
        val num = romanValue(str.charAt(0))
        sum =  sum + num
      }

      if(str.contains("VV") || str.contains("LL") || str.contains("DD"))
        throw new IllegalArgumentException(str)

      //throw new IllegalArgumentException("arg 1 was wrong...");

      def romanValue(ch : Char): Int ={
        if (ch == 'I')
          1
        else if (ch == 'V')
          5
        else if (ch == 'X')
          10
        else if (ch == 'L')
          50
        else if (ch == 'C')
          100
        else if (ch == 'D')
          500
        else if (ch == 'M')
          1000
        else
        throw new IllegalArgumentException(str)
      }

      for( i <- 0 until str.length()-1){

          val num1 = romanValue(str.charAt(i))
        if (i+1 < str.length())
        {
          val num2 = romanValue(str.charAt(i + 1));
          if (num1 >= num2)
          {
            sum = sum+(num1);
          }
          else
          {
            sum = sum + num2 - num1;
            }
        }
        else
        {
          sum = sum + num1;

        }
      }

      if(sum > 3999)
        throw new IllegalArgumentException(str)

      sum
    }

  }
