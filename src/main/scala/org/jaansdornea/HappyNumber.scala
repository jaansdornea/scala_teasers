package org.jaansdornea

import scala.annotation.tailrec

/**
 * Starting with any positive integer, replace the number by the sum of the squares of its digits, and repeat the process until the number equals 1 (where it will stay), or it loops endlessly in a cycle which does not include 1. Those numbers for which this process ends in 1 are happy numbers.

Example: 19 is a happy number

1^2 + 9^2 = 82
8^2 + 2^2 = 68
6^2 + 8^2 = 100
1^2 + 0^2 + 0^2 = 1
 * 
 */
object HappyNumber extends App {
  
  private def digits(n: Int): List[Int] = 
    (1 to n.toString.length).foldLeft(List[Int]())( (l, i) => (n % Math.pow(10, i).toInt)/Math.pow(10, i -1).toInt :: l)
  
  @tailrec
  def isHappy(n: Int, currentiteration: Int = 0, iterations: Int = 10): Boolean = {
    if (currentiteration > iterations) false
    else {
      val number = if (n < 0) n * -1 else n
      val digitsLength = n.toString.length
      val digits = this.digits(n)
      if (digits.length == 1 && digits.head == 1) true
      else {
        val (nn, currentItn1) = (digits.foldLeft(0)( (sum, d) => Math.pow(d, 2).toInt + sum), currentiteration + 1)
        isHappy(nn, currentItn1, iterations)
      }
    }
  }
  
  assert (isHappy(100))
  assert (isHappy(19))
  assert (!isHappy(37))
  
}