package org.jaansdornea
/**
 * You are a professional robber planning to rob houses along a street.
 * Each house has a certain amount of money stashed,
 * the only constraint stopping you from robbing each of them
 * is that adjacent houses have security system connected
 * and it will automatically contact the police if two adjacent houses were broken into on the same night.
 *
 * Given a list of non-negative integers representing the amount of money of each house,
 * determine the maximum amount of money you can rob tonight without alerting the police.
 *
 */
object Robber extends App {
  def doMax(houses: Array[Int]): Int = {
    val cache = scala.collection.mutable.Map[List[Int], Int]()
    def max(houses: List[Int]): Int = {
      if (cache.contains(houses)) cache(houses)
      else {
        val amount = if (houses.isEmpty) {
          cache.put(houses, 0)
          0
        } else if (houses.tail.isEmpty) {
          cache.put(houses, houses.head)
          houses.head
        } else {
          val maxrobbery = Math.max(max(houses.tail.tail) + houses.head, max(houses.tail))
          cache.put(houses,maxrobbery)
          maxrobbery
        }
        cache.put(houses, amount)
        amount
      }
    }
    max(houses.toList)
  }

  val m = doMax(Array[Int](0, 3, 2, 4, 7, 5))
  assert(m == 12)
  assert(doMax(Array[Int](1, 2, 5, 4)) == 6)
  assert(doMax(Array[Int](1, 0, 0, 5)) == 6)

}