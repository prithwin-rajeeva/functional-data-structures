package org.scala.algo

object SubsetSumProblem {
  def subsetSumExists(arr:Array[Int],sum:Int):Boolean = {
    if(sum == 0) true
    else if(arr.length == 0 || sum < 0) false
    else {
      subsetSumExists(arr.slice(1,arr.length),sum - arr(0)) || subsetSumExists(arr.slice(1,arr.length),sum)
    }
  }

  def subsetSumDP(arr:Array[Int],sum:Int):Boolean = {
    val grid = Array.ofDim(arr.length + 1,sum + 1)
    var i = 0
    while(i < grid.length) {
      var j = 0
      while(j < grid(i).length) {
//        a()()
        j += 1
      }
      i += 1
    }
    false
  }
}
