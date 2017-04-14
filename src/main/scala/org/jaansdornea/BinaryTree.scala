package org.jaansdornea

import scala.annotation.tailrec

/*
 * 
 * Given a binary tree, check whether it is a mirror of itself (ie, symmetric around its center).
 * For example, this binary tree [1,2,2,3,4,4,3] is symmetric
 * But [1,2,2,null,3,null,3] is not
 */
object BinaryTree extends App {
 
  // 1st level = 1
  // 2nd level = 2
  // 3rd level = 4
  // 4th level = 8
  // level = 2 ^ (level - 1)

  def symmetric(tree: Array[Int]): Boolean = {
    
    @tailrec
    def isPalindrome(startIndex: Int, endIndex: Int): Boolean = {
      if (startIndex >= endIndex) {
        true
      }
      else {
        tree(startIndex) == tree(endIndex) && isPalindrome(startIndex + 1, endIndex -1)
      }
    }
    @tailrec
    def checksymmetric(level: Int, startIndex: Int = 0): Boolean = {
      val endIndex = (startIndex + Math.pow(2, level).toInt - 1)
      if (tree.length - 1 < endIndex) false // cannot be symetric since isn't a vaild binary tree
      else if (tree.length -1 == endIndex) isPalindrome(startIndex, endIndex)
      else isPalindrome(startIndex, endIndex) && checksymmetric(level + 1, endIndex + 1)
    }
    
    checksymmetric(0)
  }
  
  assert(symmetric(Array(1,2,2,3,4,4,3)))
  assert(!symmetric(Array[Int](1,2,2,-1,3,-1,3)))
  
}