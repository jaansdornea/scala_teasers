package org.jaansdornea

object IsPalindrome extends App {
 
  def palindrome(i: Int): Boolean = {
    val full = i.toString
    val length = full.length()
    (0 to (length-1)).foldLeft(true) ((prev, offset) => prev && full(0 + offset) == full(length - 1 - offset))
  }
  
  assert(palindrome(1))
  assert(!palindrome(21))
  assert(palindrome(212))
  assert(palindrome(2112))
  assert(!palindrome(21123))
}