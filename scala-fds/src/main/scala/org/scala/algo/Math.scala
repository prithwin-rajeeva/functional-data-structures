package org.scala.algo

object Math {
  def !(n:Int):Int = {
    def _fact(acc:Int,num:Int):Int = {
      if(num <= 1) acc
      else _fact(acc * num, num - 1)
    }
    _fact(1,n)
  }

  def pow(x:Int,n:Int):Int = {
    if(n == 0) 1
    else if (n ==1 ) x
    else {
      if(n%2 == 0) {
        pow(x, n/2) * pow(x, n/2)
      } else {
        x * (pow(x, n/2) * pow(x, n/2))
      }
    }
  }

}
