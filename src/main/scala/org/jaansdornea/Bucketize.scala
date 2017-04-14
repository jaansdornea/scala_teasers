package org.jaansdornea

object Bucketize extends App {
  // buckets are numbered 0 => 9
   def getBucket(totalCount: Int, numBuckets: Int, index: Int): Int = {
     val amountLeftOver = totalCount % numBuckets
     val bucketSize = totalCount/numBuckets
     // case 1 equally divisible => 
     if (amountLeftOver == 0) {
       return index/bucketSize
     }
     
     else {
       val beyondWeightedSide = index - (bucketSize + 1) * amountLeftOver
       // index is on bucketSize side
       if (beyondWeightedSide <= 0) index/(bucketSize + 1) + 1
       else  amountLeftOver + (beyondWeightedSide/bucketSize)
     }
     
   }
   
   val gb0 = getBucket(15, 10, 1)
   val gb3 = getBucket(15, 10, 5)
   val gb7 = getBucket(15, 10, 12)
   val gb20 = getBucket(10, 10, 1)
   val gb4 = getBucket(10, 10, 5)
   assert (gb0 == 1)
   assert (gb7 == 7)
   assert (gb20 == 1)
   assert (gb4 == 5)
   assert (gb3 == 3)
   
}