package org.jaansdornea.utility

object BinaryHeap  {
  def apply[K: Manifest, T:Ordering:Manifest](khint: K, thint: T, pairs: (K,T)*):BinaryHeapWithMap[K,T] = {
    val hm = new BinaryHeapWithMap[K,T](khint, thint, pairs.size + 10)
    pairs.foreach{ case (k, t) => hm.insert(k, t) }
    hm
  }
}

/**
 * K = some key label type, T = some comparable value type assigned to the key stored in the heap
 */
class BinaryHeapWithMap[K: Manifest, T: Ordering: Manifest](khint: K, thint: T, size: Int) {
  import scala.annotation.tailrec
  import Ordering.Implicits._
  import func._

  private var storageH = new Array[(K, T)](size)
  private var storageM = scala.collection.mutable.Map[K, Int]()

  /*
   *  access a location using from a given value using a moveFrom function
   */
  private def access(k: K)(moveFrom: (Int) => Int): Option[(K, T)] = {
    def locationValid(i: Int): Boolean = i < storageH.length && i > -1
    val location = storageM.getOrElse(k, throw new IllegalStateException(s"no k: $k in storage"))
    val index = moveFrom(location)
    if (locationValid(index)) Some(storageH(index))
    else None
  }
  /**
   * get the left child
   */
  def leftIndex(i: Int): Int = i * 2 + 1
  def left(k: K): Option[(K, T)] = access(k)(leftIndex)
  /**
   * get the right child
   */
  def rightIndex(i: Int): Int = i * 2 + 2
  def right(k: K): Option[(K, T)] = access(k)(rightIndex)
  /**
   * get the parent
   */
  def parentIndex(i: Int): Int = (i / 2).toInt
  def parent(k: K): Option[(K, T)] = access(k)((i) => (i / 2).toInt)

  @tailrec
  private def bubbleUp(index: Int): Unit = {
    if (index == 0) /* nowhere to bubble too*/ {}
    else {
      val pi = parentIndex(index)
      if (storageH(index)._2 > storageH(pi)._2) {
        storageH <-> (index, pi)
        storageM <-> (storageH(index)._1, storageH(pi)._1)
        bubbleUp(pi)
      }
    }
  }

  @tailrec
  private def floatDown(index: Int): Unit = {
    def possibleMove(f: (Int) => Int): Boolean = {
      if (storageH(index)._2 < storageH(f(index))._2) {
        storageH <-> (index, leftIndex(index))
        storageM <-> (storageH(index)._1, storageH(leftIndex(index))._1)
        true
      } else false
    }
    if (leftIndex(index) > storageM.size) /* nowhere to go */ {}
    else {
      val floatIndex = if (possibleMove(leftIndex)) Some(leftIndex(index))
      else if (possibleMove(rightIndex)) Some(rightIndex(index))
      else None
      if (floatIndex.isDefined) floatDown(floatIndex.get)
    }
  }

  def insert(k: K, t: T): Unit = {
    // get map size to know where to put new value
    val insertionIndex = storageM.size
    if (storageH.length <= insertionIndex) storageH = storageH ++ new Array[(K, T)](storageH.length)
    storageH(insertionIndex) = (k, t)
    storageM(k) = insertionIndex
    bubbleUp(insertionIndex)
  }

  def peakMin(): Option[(K, T)] = if (storageM.size > 0) Some(storageH(0)) else None
  def popMin(): Option[(K, T)] =
    if (storageM.size == 0) None
    else {
      val popped = storageH(0)
      // get last element and place on top
      storageH(0) = storageH(storageM.size)
      // set to 0 in map
      storageM(storageH(0)._1) = 0
      // remove popped key from map
      storageM.remove(popped._1)
      // float top down
      floatDown(0)
      Some(popped)
    }

  def contains(k: K): Boolean = storageM.contains(k)
  def update(k: K, t: T): Unit = {
    val index = storageM(k)
    val previousValue = storageH(index)._2
    storageH(index) = (k, t)
    if (previousValue < t) floatDown(index)
    else if (previousValue > t) bubbleUp(index)
  }

}