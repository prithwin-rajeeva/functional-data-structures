package org.scala.algo

object Merger {
  def merge[T](left:Seq[T],right:Seq[T])(implicit o:Ordering[T]):Seq[T] = {
    if(left.isEmpty) right
    else if(right.isEmpty) left
    else if(o.lt(left.head,right.head)) merge(left.tail,right).+:(left.head)
    else if(o.gt(left.head,right.head)) merge(left,right.tail).+:(right.head)
    else merge(left.tail,right.tail).+:(left.head)
  }
}
