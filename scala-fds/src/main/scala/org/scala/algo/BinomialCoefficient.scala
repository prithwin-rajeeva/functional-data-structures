package org.scala.algo

object BinomialCoefficient {
  def C(n:Int,r:Int):Int = {
    if(n == r) 1
    else if (r < 0 || r > n) 0
    else C(n-1,r-1) + C(n-1,r)
  }

  def main(args: Array[String]): Unit = {
    println(C(11,5))
  }
}
