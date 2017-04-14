package org.jaansdornea

/**
 * Given a binary tree, find the maximum path sum.

For this problem, a path is defined as any sequence of nodes from some starting node to any node in the tree along the parent-child connections. The path must contain at least one node and does not need to go through the root.

For example:
Given the below binary tree,

This solution is N^2 can we get to an N solution...

 */

object BTreeMaxPathSum extends App {
  
case class Node (val i: Int, var left: Option[Node] = None, var right: Option[Node] = None) {
    def this(i: Int, l: Node, r: Node) = this(i, Some(l), Some(r)) 
  }
  
  // for each node want to sum up max path and take max from all nodes
  def maxForSingleNode(root: Option[Node]): Int = {
    def accumMaxDownPath(sum: Int, n: Option[Node]): Int = {
      n match {
        case None => sum
        case Some(v) => Math.max(accumMaxDownPath(sum + v.i, v.left), accumMaxDownPath(sum + v.i, v.right))
      }
    }
    root match {
      case None => 0
      case Some(r) => {
        accumMaxDownPath(0, r.left) + accumMaxDownPath(r.i, r.right)   
      }
    }
  }
  
  def maxAcrossTree(root: Option[Node]): Int = {
    root match {
      case None => 0
      case Some(r) => Math.max(maxForSingleNode(root), Math.max(maxForSingleNode(r.left), maxForSingleNode(r.right))).toInt
    }
  }

}