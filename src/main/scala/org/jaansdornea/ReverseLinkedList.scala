package org.jaansdornea

import scala.annotation.tailrec
import scala.Range

object ReverseLinkedList extends App {
  case class Node[V] (val v: V, var next: Option[Node[V]]) 
    
  def initializeList[V](vals: V*): Option[Node[V]] = {
    if (vals.isEmpty) None
    else {
      val last: Option[Node[V]] = None
      val headNode = Range(vals.length - 1, -1, -1).foldLeft(last){ (on, i) => Option(Node(vals(i), on)) }
      headNode
    }
  }
  
  /*
   * /* Function to reverse the linked list */
    Node reverse(Node node) {
        Node prev = null;
        Node current = node;
        Node next = null;
        while (current != null) {
            next = current.next;
            current.next = prev;
            prev = current;
            current = next;
        }
        node = prev;
        return node;
    }
   */
  
  def reverse(n: Node[Int]): Node[Int] = {
    
    @tailrec
    def doReverse(prev: Option[Node[Int]], curr: Node[Int]): Node[Int] = 
      curr.next match {
        case None => {
          curr.next = prev
          curr
        }
        case Some(nn) => {
          curr.next = prev
          doReverse(Some(curr), nn)
        }
      }
    
    doReverse(None, n)
  }
  
  println (reverse (initializeList(1,2,3,4,5).get))
  
}