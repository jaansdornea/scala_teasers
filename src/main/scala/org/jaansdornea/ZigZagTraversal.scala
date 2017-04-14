package org.jaansdornea

import scala.annotation.tailrec
import scala.Range

/*Given binary tree [3,9,20,null,null,15,7],
    3
   / \
  9  20
    /  \
   15   7
return its zigzag level order traversal as:
[
  [3],
  [20,9],
  [15,7]
]
*/

object ZigZagTraversal extends App {

  trait Direction
  case object LeftToRight extends Direction
  case object RightToLeft extends Direction

  case class Node(val i: Int, var left: Option[Node] = None, var right: Option[Node] = None) {
    def this(i: Int, l: Node, r: Node) = this(i, Some(l), Some(r))
  }

  def traverse(root: Node): List[List[Int]] = {
    @tailrec
    def accumTraversal(accum: List[List[Int]], direction: Direction, nodes: List[Option[Node]]): List[List[Int]] = {
      if (nodes.isEmpty) accum
      else {
        val (iteration, nextDirection) = direction match {
          case LeftToRight => ((0 until (nodes.length)), RightToLeft)
          case RightToLeft => (Range((nodes.length) - 1, -1, -1), LeftToRight)
        }
        
        val definedNodes = for (index <- iteration if nodes(index).isDefined) yield nodes(index).get
        if (definedNodes.isEmpty) accum
        else {
          val newAccum = accum :+ definedNodes.map(n => n.i).toList
          val newNodeOptions = nodes.toList.flatMap(n => if (n.isDefined) List(n.get.left, n.get.right) else List[Option[Node]]())
          accumTraversal(newAccum, nextDirection, newNodeOptions)
        }
      }
    }

    accumTraversal(List[List[Int]](), LeftToRight, List(Some(root)))
  }
  
  val _7 = Node(7)
  val _15 = Node(15)
  val _20 = Node(20, Some(_15), Some(_7))
  val _9 = Node(9)
  val root = Node(3, Some(_9), Some(_20))
  
  println(traverse(root))

}