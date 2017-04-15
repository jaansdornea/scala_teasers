package org.jaansdornea

// Given preorder and inorder traversal of a tree, construct the binary tree.
object TreeConstruction extends App {

  case class Node[V](val value: V, var left: Option[Node[V]] = None, var right: Option[Node[V]] = None)
  
  def construct(preorder: List[Char], inorder: List[Char]): Option[Node[Char]] = {

    // preorder yields parent then left then right provided there are no more left
    // start with first in preorder 
    val preorderStack = scala.collection.mutable.Stack[Char]().pushAll(preorder.reverse)
    
    def build(preo: scala.collection.mutable.Stack[Char], ino: List[Char]): Option[Node[Char]] = {
      if (ino.isEmpty) None
      else {
        val current = Node(preo.pop())
        val midpoint  = ino.indexOf(current.value)
        val leftHalf = ino.slice(0, midpoint)
        val rightHalf = ino.slice(midpoint + 1, ino.length)
        current.left  = build(preo, leftHalf)
        current.right = build(preo, rightHalf)
        Some(current)
      }
    }
    
    build(preorderStack, inorder)
  }
  
  println(construct(List('F', 'B', 'A', 'D', 'C', 'E', 'G', 'I', 'H'), List('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')))
  
}