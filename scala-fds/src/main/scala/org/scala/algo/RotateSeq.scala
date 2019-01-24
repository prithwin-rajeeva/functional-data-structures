package org.scala.algo

import scala.annotation.tailrec

object RotateSeq {
  def rotateRight[T](target:Seq[T],times:Int):Seq[T] = {
    val eTimes = times % target.length
    target.slice(eTimes,target.length) ++ target.slice(0,eTimes)
  }

  def rotateLeft[T](target:Seq[T],times:Int):Seq[T] = {
    val eTimes = (target.length) - times % target.length
    target.slice(eTimes,target.length) ++ target.slice(0,eTimes)
  }

  /**
    *  12345678 ->  56781234
    */
  def rl(arr:Array[Int],times:Int):Array[Int] = {
    @tailrec
    def _rl(acc:Array[Int],t:Int,rem:Array[Int]):Array[Int] = {
      if(t > 0) {
        _rl(acc :+ rem.head, t -1 , rem.tail)
      } else {
        rem ++ acc
      }
    }

    _rl(Array.empty[Int],times % arr.length,arr)
  }

  def main(args: Array[String]): Unit = {
    println(rl(Array(1,2,3,4,5,6,7,8),17).toList)
  }
}
