package org.scala.fds

/**
  * Defines a list trait
  * @tparam A of type A
  */
trait SList[+A] {
  def isEmpty:Boolean
  def isNonEmpty:Boolean = !isEmpty
  def <+[B>:A](item:B):SList[B]
  def contains[B>:A](item:B):Boolean
  def apply(index:Int):A
  def head:A
  def tail:SList[A]
  def <+>[B>:A](other:SList[B]): SList[B]
  def size:Int
  def removeNthNodeFromEnd(n:Int):SList[A]
  def map[B](f: A => B):SList[B]
  def flatMap[B](f: A => SList[B]):SList[B]
}

case class ::[+A](head:A,tail:SList[A]) extends SList[A] {
  def isEmpty: Boolean = false
  def <+[B>:A](item:B):SList[B] = ::(head,tail <+ item)
  def contains[B >: A](item: B): Boolean = if (head == item) true
  else
    tail.contains(item)

  def apply(index:Int):A = {
    def _apply[T](skip:Int,rem:SList[T]): T = {
      if(skip == 0) rem.head
      else _apply(skip-1, rem.tail)
    }
    if(index < 0) throw new ArrayIndexOutOfBoundsException
    _apply(index,this)
  }

  def <+>[B>:A](other:SList[B]):SList[B] = this match {
    case ::(x,Nill) => ::(x,other)
    case ::(x,xs) => ::(x,xs <+> other)
  }
  lazy val size:Int = 1 + tail.size

  override def removeNthNodeFromEnd(n: Int): SList[A] = {
    if(this.size < n) throw new IllegalArgumentException
    else if(this.size == n) tail
    else ::(head,tail.removeNthNodeFromEnd(n))
  }
  override def map[B](f: A => B): SList[B] = {
    ::(f(head),tail.flatMap(f.andThen(i => ::(i,Nill))))
  }

  override def flatMap[B](f: A => SList[B]): SList[B] = {
    f(head) <+> tail.flatMap(f)
  }
}
case object Nill extends SList[Nothing] {
  def isEmpty: Boolean = true
  def <+[B](item:B):SList[B] = ::(item,SList.empty[B])
  def contains[B](item:B):Boolean = false
  def apply(index:Int) = throw new ArrayIndexOutOfBoundsException
  def head = throw new ArrayIndexOutOfBoundsException
  def tail = throw new ArrayIndexOutOfBoundsException
  def <+>[B](other:SList[B]):SList[B] = other
  val size:Int = 0
  override def removeNthNodeFromEnd(n: Int): SList[Nothing] = throw new IllegalAccessException()
  override def map[B](f: Nothing => B): SList[B] = Nill
  override def flatMap[B](f: Nothing => SList[B]): SList[B] = Nill
}

object SList {
  def apply[A](contents: A*): SList[A] = contents.foldLeft(SList.empty[A])((x, y) => x <+ y)
  def empty[A]: SList[A] = Nill
}