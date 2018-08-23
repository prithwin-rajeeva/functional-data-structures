package org.scala.algo

object Duplication {
  def containsDuplicate(nums: Array[Int]): Boolean = {
    def _containsDuplicate(reg:Set[Int],in:Seq[Int]):Boolean = {
      if(in.isEmpty) false
      else if(reg.contains(in.head)) true
      else _containsDuplicate(reg + in.head,in.tail)
    }
    if(nums.isEmpty) false else _containsDuplicate(Set.empty[Int],nums.toList)
  }
}