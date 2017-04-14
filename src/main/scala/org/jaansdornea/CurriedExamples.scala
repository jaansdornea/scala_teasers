package org.jaansdornea

object CurriedExamples extends App {
 
  def adder(m: Int, n: Int): Int = m + n
  val adder2 = adder(2, _:Int)
  println (adder2(3))
  
  def adderr(m: Int)(n: Int): Int = m + n
  val adder3 = adderr(3) _
  
  println (adder3(4))
  
  def addAll(i: Int*): Int = 
    i.fold(0)((a, b) => a + b)
  
  println (addAll(adder3(4), adder2(3)))
}