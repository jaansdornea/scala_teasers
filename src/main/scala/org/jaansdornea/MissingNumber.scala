package org.jaansdornea
/**
 * Given an array containing n distinct numbers taken from 0, 1, 2, ..., n, 
 * find the one that is missing from the array.

For example,
Given nums = [0, 1, 3] return 2.
 */
object MissingNumber extends App {
  
  def findMissing(a: Array[Int]): Int =
    (0 until (a.length)).takeWhile((index) => index == a(index)) match {
    case r: Range  => if (r.length != 0)  r.length else 0
  }
  
  println(findMissing(Array[Int](0,1,3)))
  
}