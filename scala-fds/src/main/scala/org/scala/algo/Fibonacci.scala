package org.scala.algo

import org.scala.fds.SList

object Fibonacci {
  def get(n:Int):Long = {
    if(n <= 2) 1
    else get(n-1) + get(n-2)
  }

  def list(n:Int):List[Long] = {
    def _list(n:Int,p:Long,pp:Long,acc:List[Long]): List[Long] = {
      if(n<=0) acc
      else _list(n-1,pp,p+pp,acc :+ (p+pp))
    }
    if(n <= 2) List.fill(n)(1)
    else _list(n-2,1,1,List(1,1))
  }
}
