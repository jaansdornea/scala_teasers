package org.jaansdornea

/*
Given a string, find the first non-repeating character in it 
and return it's index. If it doesn't exist, return -1.

Examples:
s = "leetcode"
return 0.

s = "loveleetcode",
return 2.
* 
*/
object FirstNonRepeatingChar extends App {

  // add chars to a stack then pop until empty
  val counts = scala.collection.mutable.Map[String, Int]()
  def getFirst(s: String): Int = {
    val counts = s.foldLeft(Map[Char, Int]()){ case (map, c) => {
      map.get(c) match {
        case None => map + (c -> 1)
        case Some(valu) => map.updated(c, valu + 1)
      }
    }}
    val r = (0 until (s.length())).takeWhile((i) => counts.get(s(i)).get > 1)
    if (r.length == s.length()) -1
    else r.length
  }
  
  println(getFirst("leetcode"))
  println(getFirst("loveleetcode"))
  
}