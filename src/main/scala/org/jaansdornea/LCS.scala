package org.jaansdornea

object LCS extends App {
  
  case class XY(val x: Int, val y: Int)
 
  val cache = scala.collection.mutable.Map[XY, Int]() 
  
  def calculateLCS[A](X: Seq[A], Y: Seq[A]): Int = {
    
    def storeAndReturn(key: XY, value: Int): Int = {
      cache.put(key, value)
      value
    }
    
    def lcs[A](X: Seq[A], Y: Seq[A], m: Int, n: Int): Int = 
       cache.get(XY(m, n)) match {
        // already in the cache just return
        case Some(currentLength) => currentLength 
        // not in the cache, calculate
        case None => {
          // just return zero when 
          if (m < 0 || n < 0) 0 
          
          else if (X(m) == Y(n)) 
            storeAndReturn(XY(m, n), 1 + lcs(X, Y, m - 1, n - 1))   
          
          else 
            storeAndReturn(XY(m, n), 
                Math.max(lcs(X, Y, m - 1, n), lcs(X, Y, m, n - 1)))
        }
    }
          
    lcs(X, Y, X.length - 1 , Y.length - 1)
  }
  
  println(calculateLCS ("AGGTAB", "GXTXAY"))

}