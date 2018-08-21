package org.scala.algo
import scala.collection.immutable

object Permutations {

  def permuteLib[T](in:Seq[T]):Seq[Seq[T]] = in.permutations.toList

  def permute[T](in:Seq[T]):Seq[Seq[T]] = {

    def _p(lead:Seq[T],rest:Seq[T],acc:Seq[Seq[T]]): Seq[Seq[T]] = {
      if(rest.isEmpty) {
        acc :+ lead
      } else {
        val elems = for(i <- rest.indices) yield
          (rest(i),rest.slice(0,i) ++ rest.slice(i+1,rest.length))
        elems.flatMap {
          case (startChar,reminder) => _p(lead :+ startChar,reminder,acc)
        }
      }
    }

    _p(Seq.empty,in,Seq.empty)
  }
}
