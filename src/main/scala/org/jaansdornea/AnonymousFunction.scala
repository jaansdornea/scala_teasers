package org.jaansdornea

object AnonymousFunction extends App {
 
  val plusOne = (i: Int) => i + 1
  def doFunction(f: (Int) => (Int))(i: Int): Int = f(i)
  def plusThree(i: Int): Int = i + 3
  
  println( doFunction(plusOne)(0))
  println( doFunction((i) => i + 2)(0))
  println( doFunction(plusThree)(0))

}
