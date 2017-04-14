package org.jaansdornea

import scala.annotation.tailrec
import scala.Range
/**
 * Given a singly linked list, determine if it is a palindrome.

Follow up:
Could you do it in O(n) time and O(1) space?
 */
object DeterminePalindromeSLL extends App {
 case class Node[V] (val v: V, var next: Option[Node[V]]) 
  def initializeList[V](vals: V*): Option[Node[V]] = {
    if (vals.isEmpty) None
    else {
      val last: Option[Node[V]] = None
      val headNode = Range(vals.length - 1, -1, -1).foldLeft(last){ (on, i) => Option(Node(vals(i), on)) }
      headNode
    }
  } 
 
 def isPalindrome(head: Node[Char]): Boolean = {
   head.next match {
     case None => true
     case Some(_) => // have atleast two nodes so find mid point reverse and check
     {
   
       def equal(first: Node[Char], last: Node[Char]): Boolean = first.v == last.v
   
       def findMidPoint(first: Node[Char]): Node[Char] = {
         
         @tailrec
         def moveRightToMid( slow: Node[Char], fast: Option[Node[Char]]) : Node[Char] = 
           fast match {
             case None => slow             
             case Some(tail) if tail.next.isEmpty => slow
             case default => {
               val (h, nn) = (slow.next.get, fast.get.next.get.next)
               moveRightToMid(h,nn)
             }
           } 
         
         moveRightToMid(first, first.next.get.next)
       }
       
       val mid = findMidPoint(head)
       val nodeStack = scala.collection.mutable.Stack[Char]()
       
       def addToStack(first: Node[Char]) {
         nodeStack.push(first.v)
         first.next match {
           case None => {} // do nothing
           case Some(n) => addToStack(n)
         }
       }
       
       def checkAna(n: Node[Char], stack: scala.collection.mutable.Stack[Char]): Boolean = {
         if (stack.isEmpty) true
         else{
           val expected = stack.pop(); 
           if (expected != n.v) false else checkAna(n.next.get, stack)
         }
       }
       addToStack(mid)
       checkAna(head, nodeStack)
     }
   }
 }
 
 assert (!isPalindrome(initializeList('a', 'b', 'c').get))
 assert (isPalindrome(initializeList('a', 'b', 'c', 'c', 'b', 'a').get))
 assert (isPalindrome(initializeList('a', 'b', 'c', 'b', 'a').get))
 
}