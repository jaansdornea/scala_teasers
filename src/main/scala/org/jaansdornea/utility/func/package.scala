package org.jaansdornea.utility

package object func {
 
  implicit class ArraySwap[T: Manifest](a: Array[T]) {
    def <->(x: Int, y: Int): Unit = { 
      // check bounds
      Array(x, y) foreach { i => assert (i >=0 && i < a.length) }
      val temp = a(x); a(x) = a(y); a(y) = temp
    }
  } 
  
  implicit class MapSwap[T](m: scala.collection.mutable.Map[T, Int]) {
    def <-> (x: T, y: T): Unit = {
      val (xVal, yVal) = (m.get(x), m.get(y))
      (xVal, yVal) match {
        case (Some(u), Some(v)) => { m.update(x, v); m.update(y, u) }
        case default => /* one of the entries doesn't exist */ 
          throw new IllegalStateException(s"$x or $y is not present, cannot swap") 
      }
    }
  }
  
  
}