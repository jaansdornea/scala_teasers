package org.jaansdornea
//number except for multiples of three it should output “Fizz” instead of the number 
//and for the multiples of five output “Buzz”. 
//For numbers which are multiples of both three and five output “FizzBuzz”.

object FizzBizz extends App {
  
  def fizzBizz(n: Int) :String = {
    val sb = new StringBuilder("")
    if (n % 3 == 0) sb.append("Fizz")
    if (n % 5 == 0) sb.append("Buzz")
    if (sb.toString.isEmpty) n.toString else sb.toString
  }
  
  (0 to 20).foreach( (n) => println(fizzBizz(n)))
  
}