package org.jaansdornea

object KnapSack extends App {
  case class Item (weight: Int, value: Int, name: String)
  case class HomogenousItems(item: Item, count: Int) {
    def weight = item.weight * count
    def value  = item.value * count
  }
  case class HeterogeneousItems(items: Map[Item, Int] = Map[Item, Int]()) {
    def this(item: Item) = this(Map(item -> 1))
    def +(item: Item, count: Int = 1):HeterogeneousItems = 
      if (items.contains(item)) HeterogeneousItems(items.updated(item, items.get(item).get + count))
      else HeterogeneousItems(items + (item -> count))
    
    def -(item: Item, count: Int = 1): (HeterogeneousItems, Option[HomogenousItems]) = 
      items.get(item) match {
        case None => (this, None)
        case Some(k) if (k == count) => 
          (HeterogeneousItems(items - item), Some(HomogenousItems(item, count)))
        case Some(k) if (k > count) => 
          (HeterogeneousItems(items.updated(item, k - count)), Some(HomogenousItems(item, count)))
        case default => (this, None)
      }
    
    def aggregate(f:(HomogenousItems)=>Int):Int = 
      items.map( (kv) => HomogenousItems(kv._1, kv._2)).foldLeft(0)((sum, ic) => sum + f(ic))
    
    def weight = aggregate( (ic) => ic.weight)
    def value = aggregate((ic) => ic.value)
  }
  type TreasureChest = HeterogeneousItems
  case class Sack (items: HeterogeneousItems, capacityLeft: Int) {
    
    def +(item: Item, count: Int = 1): Sack =      
        capacityLeft - (item.weight * count) match {
          case cl: Int => 
            Sack(HeterogeneousItems(items.items.updated(item, {
              items.items.get(item) match {
                case None => count 
                case Some(x) => x + count
              }
            })), if (cl >= 0) cl else -1)
        }
    
    def value = if (capacityLeft >= 0 ) items.value else 0
  }
  
  /**
   * get maximum value from robbing the chest with given sack
   */
  def rob(chest: TreasureChest, sackSize: Int): Int = {
    // empty sack has no value
    val cache = scala.collection.mutable.Map[Sack, Int]((Sack(HeterogeneousItems(), sackSize), 0))
    // sack with single item has that item's value in it
    chest.items.foreach { case (item, count) => 
      if (item.weight < sackSize) cache.put(Sack(new HeterogeneousItems(item), sackSize - item.weight), item.value)
      else cache.put(Sack(new HeterogeneousItems(item), 0), 0)
    }
    
    def iterate(chest: TreasureChest, sack: Sack, level: Int): (Int, Int) = {
      println(s"iterating level $level: on chest: $chest and sack: $sack ")
      // check the max of taking the item or not taking the item
      if (chest.items.isEmpty) {
        //println(s"returning value: ${sack.value} with sack: $sack on level: ${level}")
        (sack.value, level)
      }
      else if (chest.items.size == 1 && chest.items.head._1.weight <= sack.capacityLeft){
        val filled = sack + chest.items.head._1
        val returnVal = (filled.value, level + 1) 
        //println(s"returning value: ${returnVal._1} with sack: $filled on level: ${returnVal._2}")
        returnVal
      }
      else {
        val stolenItem = chest.items.head._1
        // we know that the head exists since not empty
        val (chestWithoutNextItem, _) = chest - stolenItem
        val sackWithNextItem = sack + stolenItem
        (Math.max(iterate(chestWithoutNextItem, sack, level + 1)._1, iterate(chestWithoutNextItem, sackWithNextItem, level + 1)._1), level + 1)
      }
    }
    
    iterate(chest, Sack(HeterogeneousItems(), sackSize), 0)._1
  }
  
  
  val testItems = Array[(Item, Int)](
      (Item(1, 1, "weight: 1, value: 1"), 1),
      (Item(3, 4, "weight: 3, value: 4"), 1),
      (Item(4, 5, "weight: 4, value: 5"), 1),
      (Item(5, 7, "weight: 5, value: 7"), 1))
  val treasureChest: TreasureChest = HeterogeneousItems(testItems.toMap)
  println(s" with sack size: 7 can get treasures => ${rob(treasureChest, 7)}")
  
  
  
}