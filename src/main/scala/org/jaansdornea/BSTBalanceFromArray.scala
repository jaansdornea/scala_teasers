package org.jaansdornea

// Given an array where elements are sorted in ascending order, convert it to a height balanced BST.

object BSTBalanceFromArray extends App {

  case class Node(val i: Int, var left: Option[Node] = None, var right: Option[Node] = None) {
    def this(i: Int, l: Node, r: Node) = this(i, Some(l), Some(r))
  }

  def convert(sorted: Array[Int]): Option[Node] = {
    // find midpoint of array and add as root assume no equivalent values
    // add left side convert to root left and 
    // add right side convert to root right

    def getLeftRightCenter(s: Array[Int]): (Array[Int], Array[Int], Node) = {
      val midIndex = (s.length / 2).toInt
      val root = Node(s(midIndex))

      val left = if (midIndex > 0) s.slice(0, midIndex) else Array[Int]()
      val right = if (midIndex < s.length - 1) s.slice(midIndex + 1, s.length) else Array[Int]()
      (left, right, root)
    }

    def accum(root: Node, left: Array[Int], right: Array[Int]): Unit = {

      root.left =
        if (left.length == 0) None
        else {
          val (ll, lr, leftRoot) = getLeftRightCenter(left)
          accum(leftRoot, ll, lr)
          Some(leftRoot)
        }

      root.right =
        if (right.length == 0) None
        else {
          val (rl, rr, rightRoot) = getLeftRightCenter(right)
          accum(rightRoot, rl, rr)
          Some(rightRoot)
        }
    }

    if (sorted.length == 0) None
    else {
      val (left, right, root) = getLeftRightCenter(sorted)
      accum(root, left, right)
      Some(root)
    }

  }

  println(convert(Array(1,2,3,4,5,6,7,8,9,10)))
  
  println("")
  
  println(convert(Array(1,2,3,4,5)))
  
}