package org.jaansdornea

/*
 * Input: [7, 1, 5, 3, 6, 4]
	 Output: 5

   max. difference = 6-1 = 5 (not 7-1 = 6, as selling price needs to be larger than buying price)
 */
object PurchaseStock extends App {
  
  /**
   * if have purchase price
   * can 
   * 1.) sell
   * 2.) pass move on
   * value will be sellprice - purchase price
   * 
   * if do not have purchase price
   * can buy or pass
   */
  
  def purchase(stock: Array[Int]): Int = {
    
     def buyAndSell(buy: Option[Int], sell: Option[Int]): Int = {
       buy match {
         case None => 0
         case Some(b) => {
           sell match {
             case None => -1 * b
             case Some(s) => s - b
           }
         }
       }
     }
     
     val canPurchase = (0 until (stock.length - 1))
     canPurchase.foldLeft(0)((value, purchase) => {
       
         val maxProfitOnThisPurchase = (purchase until (stock.length)).foldLeft(0)
         { 
           case ( profit, sell) =>
           val hypotheticalProfit = stock(sell) - stock(purchase)
           if (hypotheticalProfit > profit) hypotheticalProfit 
           else profit
         }
         if (maxProfitOnThisPurchase > value) maxProfitOnThisPurchase
         else value
         
     })
  }
  
  println(purchase(Array(7, 1, 5, 3, 6, 4)))
  println(purchase(Array(7, 6, 4, 3, 1)))
}