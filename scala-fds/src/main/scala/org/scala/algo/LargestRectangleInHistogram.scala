package org.scala.algo

import scala.collection.immutable.Stack

object LargestRectangleInHistogram {
  def largestRectangleArea_old(heights: Array[Int]): Int = {
    def shortest(i:Int , j: Int): Int = {
      heights.slice(i,j+1).min
    }

    def _bp(h: List[Int]): Int = {
      (for {
        i <- h.indices
        j <- h.indices
        if j >= i
      } yield {
        shortest(i,j) * (j+1 - i)
      }).max
    }

    if(heights.isEmpty) 0 else _bp(heights.toList)
  }


  def transform(st: Stack[Int], heights: Array[Int], i: Int, max: Int,t:Int): (Int, Stack[Int]) = {
    if(st.nonEmpty && heights(st.top) > heights(i)) {
      val area = t * heights(st.top)
      transform(st.pop,heights,i,math.max(area,math.max(max, heights(i) * t)),t+1)
    } else {
      (max,st.push(i))
    }
  }


  def postProcess(st: Stack[Int],max:Int,heights: Array[Int]):Int = {
    if(st.isEmpty) max
    else {
      val i = st.top
      postProcess(st.pop, math.max(max,heights(i) * i),heights)
    }
  }

  def largestRectangleArea(heights: Array[Int]): Int = {
    val x = heights.indices.foldLeft((Int.MinValue,Stack.empty[Int]))(
      (acc,i) => acc match {
        case (max,st) =>
        if (st.isEmpty || heights(st.top) < heights(i))
          (max,st.push(i))
        else {
          transform(st,heights,i,max,1)
        }
      }
    )
    println(x._1)
    println(x._2)
    math.max(x._1,postProcess(x._2,Int.MinValue,heights))
  }

  def main(args: Array[String]): Unit = {
    println(largestRectangleArea(Array(6, 7, 5, 2, 4, 5, 9, 3)))


  }
}
