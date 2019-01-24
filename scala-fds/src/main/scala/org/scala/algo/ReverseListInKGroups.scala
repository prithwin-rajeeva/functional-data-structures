package org.scala.algo

import scala.collection.immutable.Stack

object ReverseListInKGroups {
  trait List {
    def head:Int
    def rest:List
    def append(other:List):List
  }
  case class ListOfINt(head:Int,rest:List) extends List {
    def append(other: List): List = ListOfINt(head,rest.append(other))
  }
  case object Nil extends List {
    def head: Int = throw new IllegalAccessException()
    def rest: List = throw new IllegalAccessException()

    def append(other: List): List = other
  }

  object List {
    def apply2(list: Seq[Int]): List = {
      if (list.isEmpty) Nil
      else {
        ListOfINt(list.head, apply2(list.tail))
      }
    }

    def apply(ints: Int*): List = apply2(ints)

    def reverseInKGroups(in:List,k:Int):List = {
      def _rikg(acc:List,rem:List,n:Int,res:List):List = {
        if(rem == Nil) {
          res.append(acc)
        } else {
          if(n == 0) {
            _rikg(Nil,rem,k,res.append(acc))
          } else {
            _rikg(ListOfINt(rem.head,acc),rem.rest,n-1,res)
          }
        }
      }

      _rikg(Nil,in,k,Nil)
    }
  }

  def main(args: Array[String]): Unit = {
    println(List.reverseInKGroups(List(1,2,3,4,5,6,7,8,9,10),3))
  }

}
