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


  def permute(nums: Array[Int]): List[List[Int]] = {
    def _p(
            lead:List[Int],
            rest:List[Int],
            acc:List[List[Int]]):List[List[Int]] = {
      if(rest.isEmpty) acc :+ lead
      else {
        rest.zipWithIndex.flatMap {
          case (num, i) => _p(lead :+ num, rest.slice(0,i) ++ rest.slice(i+1,rest.length),acc)
        }
      }
    }
    _p(List.empty[Int],nums.toList,List.empty[List[Int]])

  }

  def balanceParenteses(n: Int): List[String] = {
    def _balanceParenteses(buf: String, st: Int, en: Int, result: List[String]): List[String] = {
      if (st > en) result else {
        if (st == 0 && en == 0) result :+ buf
        else {
          val starters = if (st > 0) _balanceParenteses(buf + "(", st - 1, en, result) else result
          if (en > 0) _balanceParenteses(buf + ")", st, en - 1, starters) else starters
        }
      }

    }

    _balanceParenteses("", n, n, List.empty)
  }

  def bizzaroBalanceParenteses(n: Int): List[String] = {
    def _balanceParenteses(buf: String, st: Int, en: Int, result: List[String]): List[String] = {
      if (st < en) result else {
        if (st == 0 && en == 0) result :+ buf
        else {
          val starters = if (st > 0) _balanceParenteses(buf + "(", st - 1, en, result) else result
          if (en > 0) _balanceParenteses(buf + ")", st, en - 1, starters) else starters
        }
      }

    }

    _balanceParenteses("", n, n, List.empty)
  }

  def main(args: Array[String]): Unit = {
    println(permute(Array(1,2,3,4)))
  }
}
