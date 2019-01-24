package org.scala.algo

import scala.collection.mutable

object LongestCommonSubsequence {
  type Input = (String,String)
  val reg = mutable.HashMap.empty[Input,Int]

  def memGet(thiz:String,that:String):Int = {
    if(reg.contains((thiz,that))) {
      println(s"hit $thiz $that")
      reg((thiz,that))}
    else {
      reg((thiz,that)) = measure(thiz,that)
      reg(thiz,that)
    }
  }
  def measure(thiz:String,that:String):Int = {
    if(thiz.isEmpty || that.isEmpty) 0
    else {
      if(thiz.head == that.head) {
        1 + memGet(thiz.substring(1),that.substring(1))
      } else {
        math.max(memGet(thiz.substring(1),that),memGet(thiz,that.substring(1)))
      }
    }
  }

  def measureDp(thiz: String, that: String): Int = {
    val grid = Array.ofDim[Int](thiz.length+1,that.length+1)
    var i = 1
    while(i < grid.length) {
      var j = 1
      while (j < grid(i).length) {
        if(i == 0 || j == 0) grid(i)(j) = 0
        else if(thiz.charAt(i-1) == that.charAt(j-1)) {
          grid(i)(j) = 1 + grid(i-1)(j-1)
        } else {
          grid(i)(j) = math.max(grid(i-1)(j),grid(i)(j-1))
        }
        j += 1
      }
      i += 1
    }
    grid(grid.length-1)(grid(0).length-1)
  }
}
