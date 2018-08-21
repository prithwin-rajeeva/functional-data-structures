package org.scala.algo

object Pallindrome {
  def isPalindrome[T](in:Seq[T]):Boolean = {
    if(in.isEmpty || in.length == 1) true
    else if(in.head == in.last && isPalindrome(in.slice(1,in.length-1))) true
    else false
  }
}
