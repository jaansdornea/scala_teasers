package org.jaansdornea

import scala.Range
/**
 * 
 * Write a function to delete a node (except the tail) in a singly linked list, given only access to that node.

Supposed the linked list is 1 -> 2 -> 3 -> 4 and you are given the third node with value 3, 
the linked list should become 1 -> 2 -> 4 after calling your function.
 * 
 */
object DeleteNodeInLL extends App {
  
  case class Node[V] (val v: V, var next: Option[Node[V]]) 
    
  def initializeList(vals: Int*): Option[Node[Int]] = {
    if (vals.isEmpty) None
    else {
      val last: Option[Node[Int]] = None
      val headNode = Range(vals.length - 1, -1, -1).foldLeft(last){ (on, i) => Option(Node(vals(i), on)) }
      headNode
    }
  }
  
  def delete[V](node: Node[V], index: Int): Option[Node[V]] = {
    index match {
      case i: Int if i < 1 => Some(node)
      case i: Int if (i == 1) => node.next
      case default => {
        val head:Option[Node[V]] = Some(node)
        val previous:Option[Node[V]] = (1 until index - 1).foldLeft(head)( (n, i) => 
          n match { 
            case None => None
            case Some(link) => link.next
          })
        previous match {
          case None => head
          case Some(n) => {
            // now replace next
            n.next match {
              case None => head
              case Some(nextn) => {
                n.next = nextn.next
                head
              }
            }
          }
        }
      }
    }
  }
  
  def _123(): Option[Node[Int]] = initializeList(1,2,3)
  
  println(delete(_123().get, 1))
  println(delete(_123().get, 2))
  println(delete(_123().get, 3))
  
}