package org.jaansdornea

import scala.annotation.tailrec

/*
Example 1:
nums1 = [1, 3]
nums2 = [2]

The median is 2.0
Example 2:
nums1 = [1, 2]
nums2 = [3, 4]

The median is (2 + 3)/2 = 2.5
*/

object MedianTwoSortedArrays extends App {
  def apply(a: Array[Int], b: Array[Int]): Double = {
    
    @tailrec
    def merge(l1: List[Int], l2: List[Int], merged: List[Int]): List[Int] = 
      if (l1.isEmpty) merged ::: l2
      else if (l2.isEmpty) merged ::: l1
      else {
        val (first, second, agg) = {
          if (l1.head > l2.head) (l1, l2.tail, merged :+ l2.head)
          else (l1.tail, l2, merged :+ l1.head)
        }
        merge(first, second, agg)
      }
    
    
    def medianOfMergedList(l: List[Int]): Double =
      if (l.isEmpty) throw new IllegalArgumentException("expect non empty input")
      else if (l.size % 2 == 0) 
      {
        val top = l.size / 2 // if size == 4 => 4/2 = 2 [0, 1] & [2, 3] thus want index 1 & 2
        val bottom = top - 1
        (l(top) + l(bottom))/ 2.0
      }
      else {
        val index = (l.size/ 2).toInt
        l(index) // if size == 3 => 3/2 = 1.5 => 2 - 1
      }
    
    medianOfMergedList(merge(a.toList, b.toList, List[Int]()))  
  }
  
  println(MedianTwoSortedArrays(Array[Int](1,3), Array[Int](2)))
  println(MedianTwoSortedArrays(Array[Int](1,2), Array[Int](3,4)))
}