package org.jaansdornea

import scala.annotation.tailrec

object PascalsTriangle extends App {
 //Given numRows, generate the first numRows of Pascal's triangle.
 // For example, given numRows = 5, 
 /*
  * [
  
     [1],
     [1,1],
     [1,2,1],
     [1,3,3,1],
     [1,4,6,4,1]
] 
* 
*/
  
  def next(current: Array[Int]): Array[Int] = {
    if (current.length == 0 ) Array(1)
    else {
      (0 to current.length).map((index) => 
        // 1.) no previous exists current
        if (index == 0) current(index)
        // 2.) exists previous no current
        else if (index == current.length ) current(index -1)
        // 3.) exists previous and current
        else current(index) + current(index -1)
      ).toArray
    }
  }
  
  def generate(row: Int): Array[Int] = {  
    @tailrec
      def accum(count: Int, current: Array[Int]): Array[Int] = {
        if (count == 0) current
        else {
          val (nextCount, nextRow) = (count - 1, next(current))
          accum(nextCount, nextRow)
        }
      }
    
    if (row == 0) Array()
    else if (row == 1) Array(1)
    else accum(row -1, Array(1))
    
  }
  
  println(generate(5).mkString(","))
  
}