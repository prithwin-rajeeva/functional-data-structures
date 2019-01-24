package org.scala.algo

import scala.annotation.tailrec

object MaxSubarry {
  def maxSubArray(nums: Array[Int]): Int = {
    nums.toList.foldLeft((0,Int.MinValue))((acc,item) => {
      val rs = if(acc._1 <= 0) 0 else acc._1
      (rs + item , math.max(acc._2,rs + item))
    } )._2
  }

  def maxSubarry(nums: List[Int]): Int = {
    @tailrec
    def maxSubarraySum(largestNumber: Int,
                       localMax: Int,
                       remainingElements: List[Int],
                       allNumberAreNegetive: Boolean,
                       largestNegetiveNumber: Int
                      ): Int = {
      if(remainingElements.isEmpty) {
        if (allNumberAreNegetive) largestNegetiveNumber else largestNumber
      } else {
        if (localMax + remainingElements.head < 0)
          maxSubarraySum(
            math.max(largestNumber, localMax),
            0,
            remainingElements.tail,
            if (remainingElements.head > 0)
              false
            else
              true,
            if (remainingElements.head > largestNegetiveNumber)
              remainingElements.head
            else largestNegetiveNumber
          )
        else {
          maxSubarraySum(
            math.max(largestNumber,
              localMax + remainingElements.head),
            localMax + remainingElements.head,
            remainingElements.tail,
            if (remainingElements.head > 0)
              false
            else
              true,
            if (remainingElements.head > largestNegetiveNumber)
              remainingElements.head
            else
              largestNegetiveNumber
          )
        }
      }
    }

    maxSubarraySum(0, 0, nums, allNumberAreNegetive = true, Int.MinValue)
  }

  def main(args: Array[String]): Unit = {
    println(maxSubarry(List(-10,-10,-10)))
  }
}


object Solution {
  def maxSubArray(nums: Array[Int]): Int = {
    @tailrec
    def _m(max: Int, maxTillHere: Int, rem: List[Int]): Int = {
      if (rem.isEmpty) max
      else {
        val here = rem.head
        if (maxTillHere + here < 0) _m(math.max(max, maxTillHere), 0, rem.tail)
        else {
          _m(math.max(max, maxTillHere + here), maxTillHere + here, rem.tail)
        }
      }
    }
    _m(0, 0, nums.toList)
  }
}
