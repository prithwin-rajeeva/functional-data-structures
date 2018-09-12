package org.scala.algo

object MaxSubarry {
  def maxSubArray(nums: Array[Int]): Int = {
    nums.toList.foldLeft((0,Int.MinValue))((acc,item) => {
      val rs = if(acc._1 <= 0) 0 else acc._1
      (rs + item , math.max(acc._2,rs + item))
    } )._2
  }

  def main(args: Array[String]): Unit = {
    maxSubArray(Array(1,2,3,4,5))
  }
}
