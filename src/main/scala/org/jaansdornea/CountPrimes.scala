package org.jaansdornea
/*
 * Description:
 *   Count the number of prime numbers less than a non-negative number, n.
 */
object CountPrimes extends App {
  
  val notPrimeCache = scala.collection.mutable.Set[Int]()
  
  // intentionally skip 1 and 2
  def isPrime(k: Int): Boolean = 
    if (notPrimeCache(k)) false // checked before
    else 
      if (k % 2 == 0) false // divisible by two
      else {
        val r = (3 to (k/2))
        if (r.takeWhile((i) => k % i != 0).size == r.size) true
        else {
          notPrimeCache.add(k)
          false
        }
      }
  
  def countPrimes(n: Int):Int = 
    n match {
      case i: Int if i < 3 => i
      case default => (3 to n).foldLeft(2)( (pt, num) => if (isPrime(num)) pt + 1 else pt)
    }
  
  assert(countPrimes(10) == 5)
  assert(countPrimes(23) == 10)
  assert(countPrimes(1) == 1)
  assert(countPrimes(2) == 2)
  
  
}