package org.jaansdornea

object ContainsDuplicate extends App {
  
  def dup(a: Array[Int]): Boolean = {
    (0 until (a.length)).foldLeft(Map[Int, Int]())({
      case (m, i) => m.get(a(i)) match {
        case None => m + (a(i) -> 1)
        case Some(x) => m.updated(i, 2)
      }
    }).foreach { case (k, v) => if (v == 2) return true }
    return false
  }
  
  assert(dup(Array[Int](1,2,2,3)))
  assert(dup(Array[Int](1,2,3,4,5,6,7,8,1)))
  
}