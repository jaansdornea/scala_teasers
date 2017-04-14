package org.jaansdornea

object Robber2 extends App {
 
  def rob(houses: Array[Int]): Int = {
    
    // case if houses is empty => 0
    // case if houses.tail is empty => houses.head
    // else max (previous + head + rob(houses.tail.tail), previous + rob(houses.tail))
    
    val cache = scala.collection.mutable.Map[List[Int], Int]()
    
    def addAndReturn(key: List[Int], value: Int): Int = 
      cache.get(key) match {
        case None => cache.put(key, value); value
        case Some(v) => v
      }
    
    
    def doRob(houses: List[Int]): Int = 
      houses match {
        case Nil => addAndReturn(Nil, 0)
        case h :: Nil => addAndReturn(h :: Nil, h)
        case h :: street => addAndReturn(houses, Math.max(doRob(street.tail) + h, doRob(street)))
      }
    
    doRob(houses.toList)
  }
  
  val m = rob(Array[Int](0, 3, 2, 4, 7, 5))
  assert(m == 12)
  assert(rob(Array[Int](1, 2, 5, 4)) == 6)
  assert(rob(Array[Int](1, 0, 0, 5)) == 6)
  
}