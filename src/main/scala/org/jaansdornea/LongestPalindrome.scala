package org.jaansdornea

import scala.annotation.tailrec

object LongestPalindrome extends App {
  /*
  def palindrome(s: String): Int = {
    def printMatrix(a: Array[Array[Int]]): Unit = {
      println
      a.foreach { row => row.foreach { element => print(s"$element ")}; println }
      println
    }
    
    val length = s.length
    val matrix = Array.ofDim[Int](length, length)
    
    (0 until length).foreach { i => matrix(i)(i) = 1 }
    
    (2 to length).foreach { sublength => 
      print(s"processing sublength: $sublength")
      (0 to (length - sublength)).foreach { startIndex => 
        println(s"processing at startIndex: $startIndex")
        
        
        matrix(startIndex)(startIndex + sublength -1) = 
          if (s(startIndex) == s(startIndex + sublength - 1)){ 
            2 + matrix(startIndex + 1)(startIndex + sublength - 2)
          } else {
            Math.max(matrix(startIndex)(startIndex + sublength -2), 
                matrix(startIndex + 1)(startIndex + sublength -1))
          }
        
        printMatrix(matrix)
      }
    }
    matrix(0)(length - 1)
  }*/
  
  def palindrome(s: String): Int = {
    case class Mark(val start: Int, val end: Int)
    type Cache = Map[Mark, Int]
    
    def initializeCache(length: Int): Cache = {
      (0 until (length)).foldLeft(Map[Mark, Int]()){ case (cache, i) => cache + (Mark(i, i) -> 1) } 
    }
    
    def handle(mark: Mark, cache: Cache = initializeCache(s.length)): Cache = 
      mark match {
        
        case Mark(start, end) if s(start) == s(end) => {
          val behind = cache.get(Mark(start, end -1))
          val updatedPLength = behind.get + 2
          
          cache + (Mark(start, end) -> updatedPLength)
        }
        
        case Mark(start, end) => {
          // Math.max(matrix(startIndex)(startIndex + sublength -2), 
          //      matrix(startIndex + 1)(startIndex + sublength -1))
          
          val below = cache.get(Mark(start+1, end - 1))
          val behind = cache.get(Mark(start, end - 2))
          
          cache + (Mark(start, end) -> 
            Math.max(below.get, behind.get))
        
        }
      }
      
    @tailrec
    def recHandle(lengths: Seq[Int], cache: Cache): Cache = {
      if (lengths.isEmpty) cache
      else {
        val updatedCache = 
          (0 to (s.length - lengths.head)).foldLeft(cache)((cache, i) => 
            {
              println(s"handle(Mark($i, ${i + lengths.head - 1}), cache)")
              handle(Mark(i, i + lengths.head - 1), cache)
            })
        recHandle(lengths.tail, updatedCache)
      }
    }
    
    recHandle(2 to (s.length), initializeCache(s.length)).get(Mark(0, s.length -1)).get
  }
  
  print(palindrome("agbdba"))
  
}