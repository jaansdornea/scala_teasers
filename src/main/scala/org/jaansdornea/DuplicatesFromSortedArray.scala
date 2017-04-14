package org.jaansdornea

import scala.annotation.tailrec

object DuplicatesFromSortedArray extends App {
 
  def removeDups(sa: List[Int]):List[Int] = {
    @tailrec
    def accumUnique(rest: List[Int], unique: List[Int]): List[Int] =
      if (rest.isEmpty) unique.reverse
      else if (unique.head == rest.head) accumUnique(rest.tail, unique)
      else accumUnique(rest.tail, rest.head :: unique)
    accumUnique(sa, List[Int]())
  }
  
  def removeDups2(sa: Array[Int]):Array[Int] = {
   if (sa.isEmpty) sa
   else {
     // temp variable for head
     // need to ensure different from index 0 
     var head = sa(0) + 1 
     
     val updateCheckAndMap: (Int) => Option[Int] = (saValue) => {
       if (saValue == head) None 
       else { 
         head = saValue
         Option(saValue)
       }
     }
     sa.map(updateCheckAndMap).filter { case Some(x) => true; case None => false }.map { case Some(x) => x }
   }
  }
  
  println (removeDups2(Array(1,2,2,3,4,4,4,4,5)).toList)
  
  
}