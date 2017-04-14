package org.jaansdornea

object ArrayIntersection extends App {
  
  /**
   * Example:
	 *  Given nums1 = [1, 2, 2, 1], nums2 = [2, 2], return [2, 2].
   *
   */
  def intersection(a: Array[Int], b: Array[Int]): Array[Int] = {
    def accum(intersection: List[Int], arest: List[Int], brest: List[Int]): List[Int] = {
      if (arest.isEmpty || brest.isEmpty) intersection
      else {
        val upToAHeadInB = brest.takeWhile((c) => c != arest.head) 
        if (upToAHeadInB.length != brest.length) 
          accum(intersection :+ arest.head, arest.tail, upToAHeadInB ::: brest.slice(upToAHeadInB.length, brest.length))
        else accum(intersection, arest.tail, brest)
      }
    }
    accum(List[Int](), a.toList, b.toList).toArray
  }
  val intersectionOf = intersection(Array[Int](1,2,2,1), Array[Int](2,2))
  println(intersectionOf)
  
}