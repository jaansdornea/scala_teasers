package org.jaansdornea

import scala.annotation.tailrec

object Solution extends scala.App {
 
  def solution(a: Array[Int]): Int = {
   case class IndexdValue(val value: Int,val index: Int)
   case class Traversal(val left: Option[IndexdValue], val bottom: Option[IndexdValue], val right: Option[IndexdValue])
   
   // index is on the outside
   val aWithIndex:List[IndexdValue] = a.zipWithIndex.toList.map{ case (index, value) => IndexdValue(index, value) }
   
   // rest = (val, index) 
   // accum = (start, low, end)
   // dir = (-1 left, 0 bottom, 1 right)
   @tailrec
   def markRange(current: Traversal, rest: List[IndexdValue], accum: List[Traversal]): List[Traversal] = {
     println (s"current: $current , rest: $rest , accum: $accum")
     if (rest.isEmpty) 
       return current match {
         case Traversal(Some(start), Some(mid), Some(end)) => Traversal(Some(start), Some(mid), Some(end)) :: accum 
         case default => accum
       }
     
     val head:IndexdValue = rest.head
     val traversal = current match {
       case Traversal(None, None, None) => {
         // assume current is the start
         (Traversal(Some(rest.head), None, None), rest.tail, accum)
       }
       case Traversal(Some(start), None, None) => {
         // moving right
         if (head.value < start.value) (Traversal(Some(start), Some(head), None), rest.tail, accum)
         else (Traversal(Some(head), None, None), rest.tail, accum)
         
       }
       case Traversal(Some(start), Some(mid), None) => {
         if (head.value < mid.value) (Traversal(Some(start), Some(head), None), rest.tail, accum)
         else (Traversal(Some(start), Some(mid), Some(head)), rest.tail, accum)
             
       }
       case Traversal(Some(start), Some(mid), Some(end)) => {
         if (head.value > end.value) {
           (Traversal(Some(start), Some(mid), Some(head)), rest.tail, accum)
         }
         else {
           (Traversal(Some(end), None , None), rest, Traversal(Some(start), Some(mid), Some(end)) :: accum)
         }
       }
     }
     return markRange(traversal._1, traversal._2, traversal._3)
   }
   
   
   def depth(start: Int, bottom: Int, end: Int): Int = {
     Math.min(start, end) - bottom
   }
   
   val pits:List[Traversal] = markRange(Traversal(None, None, None), aWithIndex, List.empty)
   
   pits.foldLeft(-1)(
       (a, b) => { 
         val dpb = depth(b.left.get.value, b.bottom.get.value, b.right.get.value)
         Math.max(a, dpb)
       })
 }
  
 val test1 = Array[Int](1,0,3,4,5,4,3,-1,2,0,1,2,1,2,1,24,5,3,5,6,7,8,6) 
 print (solution(test1))
}