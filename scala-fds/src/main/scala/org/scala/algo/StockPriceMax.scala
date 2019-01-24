package org.scala.algo

object StockPriceMax {

  def maxProfit(prices: Array[Int]): Int = {
    prices.headOption.map(
      head => prices.tail.foldLeft((head,Int.MinValue))(
        (acc,i) => acc match {
          case (min,maxp) => (math.min(min,i),math.max(maxp,i-math.min(min,i)))
        }
      )._2
    ).getOrElse(0)
  }
}
