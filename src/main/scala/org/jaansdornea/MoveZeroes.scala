package org.jaansdornea

import scala.Range

/**
 Given an array nums, write a function to move all 0's 
 to the end of it while maintaining the relative order of the non-zero elements.

For example, given nums = [0, 1, 0, 3, 12], after calling your function, nums should be [1, 3, 12, 0, 0].
*/
object MoveZeroes extends App {

  def moveZeroes(nums: Array[Int]):Unit = {
    Range(nums.length -1, -1, -1).foreach { (i) =>
      println(i)
      var j = i
      if (nums(j) == 0)
        while (j < nums.length -1 && nums(j + 1) != 0) {
          val temp = nums(j + 1)
          nums(j + 1) = 0
          nums(j) = temp
          j = j + 1
        }
    }
  }
  
  val toMove = Array[Int](0,1,0,3,12)
  moveZeroes(toMove)
  println(toMove.toList)
  
}