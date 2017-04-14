package org.jaansdornea
/*
 * Given a binary tree, find its maximum depth.
		The maximum depth is the number of nodes along the longest path
		from the root node down to the farthest leaf node.
 * 
 */
object BinaryTreeDepth extends App {
  
  case class Node (val i: Int, var left: Option[Node] = None, var right: Option[Node] = None) {
    def this(i: Int, l: Node, r: Node) = this(i, Some(l), Some(r)) 
  }
  
  def maxDepth(root: Node): Int = {
    def accum(next: Node, depth: Int): Int = 
      (next.left, next.right) match { 
        case (None, None) => depth + 1 // + 1 for this level
        case (Some(l), Some(r)) => Math.max(accum(l, depth + 1), accum(r, depth + 1))
        case (Some(l), None) => accum(l, depth + 1)
        case (None, Some(r)) => accum(r, depth + 1)
      }
    accum(root, 0)
  }
  
  val _13 = Node(13)
  val _11 = Node(11)
  val _12 = new Node(12, _11, _13)
  val _15 = Node(15)
  val _14 = new Node(14, _12, _15)
  val _7  = Node(7)
  val _10 = new Node(10, _7, _14)
  val _5  = Node(5)
  val _6  = new Node(6, _5, _10)
  val _20 = Node(20)
  val root = new Node(17, _6, _20)
  
  println (maxDepth(root)) 
  
}