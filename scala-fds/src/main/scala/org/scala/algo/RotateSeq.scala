package org.scala.algo

object RotateSeq {
  def rotateRight[T](target:Seq[T],times:Int):Seq[T] = {
    val eTimes = times % target.length
    target.slice(eTimes,target.length) ++ target.slice(0,eTimes)
  }

  def rotateLeft[T](target:Seq[T],times:Int):Seq[T] = {
    val eTimes = (target.length) - times % target.length
    target.slice(eTimes,target.length) ++ target.slice(0,eTimes)
  }
}
