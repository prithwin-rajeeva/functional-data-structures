package org.scala.algo

object LongestCommonSubsequence {
  def measure(thiz: String, that: String): Int = {
    if (thiz == "" || that == "") 0
    else if (thiz.charAt(0) == that.charAt(0)) 1 + measure(thiz.substring(1), that.substring(1))
    else math.max(measure(thiz.substring(1), that), measure(thiz, that.substring(1)))
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
