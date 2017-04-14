package org.jaansdornea

import scala.annotation.tailrec

object ReverseInt extends App {
 
  // assume positive
  def reverse(num: Int): Int = {
    val isNegative = num < 0
    
    def lengthOf(num: Int, count: Int): Int = {
      val absNum = if (num < 0) -1 * num else num
      if ( absNum == 0) count
      else if ( absNum < 10) count + 1
      else {
        val newNum = absNum / 10
        lengthOf(newNum, count + 1)
      }
    }
     
    val lengthOfReversed = lengthOf(num, 0)
    
    @tailrec
     def accum(current: Int, reversed: Int, count: Int): Int = {
       if (current == 0) reversed
       else {
         val lastDigit = current % 10
         val accumReversed = (lastDigit * Math.pow(10, lengthOfReversed - (1 + count))).toInt + reversed 
         val newCurrent = ((current - (current % 10))/10).toInt
         val newCount = count + 1
         accum(newCurrent, accumReversed, newCount)
       }
     }
     isNegative match {
       case true => accum (num, 0, 0)
       case false => -1 * accum(num, 0, 0)
     }
     accum (num, 0, 0)
  }
  
  assert (reverse(15) == 51)
  assert (reverse(18289494) == 49498281)
  assert (reverse(150) == 51)
  assert (reverse(-545453) == -354545)
  
}