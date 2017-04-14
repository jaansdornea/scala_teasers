package org.jaansdornea

object IsAnagram extends App{
  
  def ana(s: String, t: String): Boolean = {
    val (sleng, tleng) = (s.length, t.length)
    if (sleng == tleng && sleng != 0) {
      val smap = s.foldLeft(Map[Char, Int]()) { case (m, c) => 
        m.get(c) match {
          case None => m + (c -> 1)
          case Some(n) => m.updated(c, n + 1)
        }
      }
      val tmap = t.foldLeft(smap) { case (m, c) => 
        m.get(c) match {
          case None =>  return false // short circuit since all of t isn't in m
          case Some(n) => m.updated(c, n-1)
        }
      }
      smap.keySet.size == 
        tmap.takeWhile( (keyValue) => keyValue._2 == 0).size
    }
    else false
  }
  
  assert (ana("cat", "tac"))
  assert (!ana("caat", "tac"))
  assert (!ana("coat", "cast"))
  
  
}