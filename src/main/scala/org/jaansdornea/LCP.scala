package org.jaansdornea

import scala.annotation.tailrec

//Write a function to find the longest common prefix string amongst an array of strings.
object LCP extends App {
 
  @tailrec
  def lcp(submissions: List[String], prefix: String=""): String = {
    if (submissions.isEmpty) ""
    else {
      val first = if (submissions.head.isEmpty) "" else submissions.head.head
      if (submissions.length != (1 until (submissions.length)).takeWhile(submissions(_).head == first).size + 1) prefix
      else lcp(submissions.map( _.tail), prefix + first)
    }
  }  
   
  assert(lcp(List("party", "people", "phillip")) == "p")
  assert(lcp(List("the", "theworld", "hello")) == "")
  assert(lcp(List("never", "neverever")) == "never")
  
}