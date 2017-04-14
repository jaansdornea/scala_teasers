package org.jaansdornea

/*
 * Input: amount = 5, coins = [1, 2, 5]
Output: 4
Explanation: there are four ways to make up the amount:
5=5
5=2+2+1
5=2+1+1+1
5=1+1+1+1+1
 * 
 */
object CoinChange extends App {

  def countChange(money: Int, coins: List[Int]): Int = {
   // if money == 0 , 1 way no coins
   // if money left && coins.isEmpty, 0 ways, cannot provide coins
   // else countChange(money - coins.head, coins) + countChange(money, coins.tail) // add the coin or not
    
   if (money == 0) 1
   else if (money > 0 && !coins.isEmpty)
     countChange(money - coins.head, coins) + countChange(money, coins.tail)
   else 0
  }
  
  println(countChange(5, List(1,2,5)))

}