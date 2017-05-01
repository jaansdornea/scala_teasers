package org.jaansdornea

import org.jaansdornea.utility.SearchAbleTree

object OptimalBinaryTree extends App {

  case class SearchFrequency(val value: Int, val freq: Int)
  case class RootAndRange(val root: Int, start: Int, end: Int)
  var cache = scala.collection.mutable.Map[RootAndRange, Int]()
  
  def optimal(frequencies: Array[SearchFrequency]): SearchAbleTree[Int] = {

    // initialize first with single node in tree
    (0 until (frequencies.length)).foreach((i) =>
      cache.put(RootAndRange(i, i, i), frequencies(i).freq))

    // we need to take the base sum of search frequencies for nodes being calculated for
    def freqSum(r: Range): Int = r.fold(0)((sum, i) => frequencies(i).freq + sum)

    // helper function to determine node indice ranges in left and right subtrees from root
    def subTreeRanges(start: Int, endInclusive: Int, rootIndex: Int): (Option[Range], Option[Range]) =
      if (rootIndex != start && rootIndex != endInclusive)
        (Some((start) to (rootIndex - 1)), Some((rootIndex + 1) to (endInclusive)))
      else if (rootIndex == start && rootIndex == endInclusive) (None, None)
      else if (rootIndex == start) (None, Some((start + 1) to (endInclusive)))
      else (Some((start) to (endInclusive - 1)), None)

    // recursive function for determining the minCost of tree
    def minCost(start: Int, end: Int): Int = {
      if (end < start) 0
      else if (end == start) frequencies(start).freq
      else {
        val startToEnd = (start to end)
        val beginningSum = freqSum(startToEnd)
        val minCostValue: Int = startToEnd.fold(Int.MaxValue)((minValue, i) => {
          val minAtI = {
            if (!cache.contains(RootAndRange(i, start, end))) 
              cache.put(RootAndRange(i, start, end), subTreeRanges(start, end, i) match {
                case (Some(l), Some(r)) => beginningSum + minCost(l.head, l.lastElement) + minCost(r.head, r.lastElement)
                case (Some(l), None)    => beginningSum + minCost(l.head, l.lastElement)
                case (None, Some(r))    => beginningSum + minCost(r.head, r.lastElement)
                case (None, None)       => beginningSum // redundant, since end == start 
              })
            cache.get(RootAndRange(i, start, end)).get
          }
          if (minValue < minAtI) minValue else {
            println(s"minAtI=$minAtI => (root=$i, start=$start, end=$end)")
            minAtI
          }
        })
        minCostValue
      }
    }
    val optimalCost = minCost(0, frequencies.length - 1)
    ???
    
  }

  /*
  println( 
      optimal( Array[SearchFrequency](
          SearchFrequency(10,4), SearchFrequency(12,2), SearchFrequency(16,6),  SearchFrequency(21,3))))
  */
}