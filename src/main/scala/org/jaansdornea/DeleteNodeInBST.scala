package org.jaansdornea

/*
 * Given a root node reference of a BST and a key, 
 * delete the node with the given key in the BST. 
 * Return the root node reference (possibly updated) of the BST.
 * 
 */
object DeleteNodeInBST extends App {
  case class Node (var i: Int, var left: Option[Node] = None, var right: Option[Node] = None) {
    def this(i: Int, l: Node, r: Node) = this(i, Some(l), Some(r)) 
  }
  
  def deleteNode(root: Option[Node], value: Int): Unit = {
    def findMin(root: Option[Node]): Option[Node] = {
      root match {
        case None => None
        case Some(n) => {
          if (n.left.isDefined) findMin(n.left)
          else if (n.right.isDefined) findMin(n.right)
          else root
        }
      }
    }
    // if root ! defined return
    root match {
      case None => return
      case Some(rv) => {
        if (rv.i < value) deleteNode(rv.right, value)
        else if (rv.i > value) deleteNode(rv.left, value)
        else {
          // rv.i == value
          // replace rv.i with min on right side and remove min from tree
          var minNode = findMin(root)
          if (minNode.isDefined) {
            root.get.i = minNode.get.i
            minNode = None
          }
        }
      }
    }
  }
}