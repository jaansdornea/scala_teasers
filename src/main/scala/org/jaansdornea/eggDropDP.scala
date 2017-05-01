package org.jaansdornea

object eggDropDP extends App{
 
  case class EggDrop(eggs: Int, floors: Int)
  
  def minAttempts(eggDrop: EggDrop): Int = {
    var cache = scala.collection.mutable.Map[EggDrop, Int]()
    
    // fill the cache for 1 egg and 0 floors
    (1 to (eggDrop.floors)).foreach ( (floor) => cache.put(EggDrop(eggDrop.eggs,floor), floor))
    (1 to (eggDrop.eggs)).foreach ( (egg) => cache.put(EggDrop(egg, 0), 0))
    
    def iterate(eggDrop: EggDrop): Int = {
      cache.get(eggDrop) match {
        case Some(x) => x
        case None => {
          val minFloor = for (floor <- 1 to (eggDrop.floors)) yield {
            Math.max(1 + iterate(EggDrop(eggDrop.eggs -1, eggDrop.floors -1)),
              iterate(EggDrop(eggDrop.eggs, eggDrop.floors - floor)))
          }
          val min = minFloor.min
          cache.put(eggDrop, min)
          min
        }
      }
    }
    iterate(eggDrop)
  }
  
  println(minAttempts(EggDrop(2, 6)))
  
}