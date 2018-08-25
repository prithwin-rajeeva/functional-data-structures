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
  def isPalindrome:Boolean
  def reverse:SList[A]
  def foldLeft[B >: A](z:B)(f: (A,B) => B):B
  def foldRight[B >: A](z:B)(f: (A,B) => B):B
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
  override def isPalindrome: Boolean = {
    SList.areSame(this,this.reverse)
  }
  override def reverse: SList[A] = tail.reverse <+ head
  override def foldLeft[B >: A](z: B)(f: (A, B) => B): B = {tail.foldLeft(f(head,z))(f)}
  override def foldRight[B >: A](z: B)(f: (A, B) => B): B = f(head,tail.foldRight(z)(f))
}

case object Nill extends SList[Nothing] {
  override def isEmpty: Boolean = true
  override def <+[B](item:B):SList[B] = ::(item,SList.empty[B])
  override def contains[B](item:B):Boolean = false
  override def apply(index:Int) = throw new ArrayIndexOutOfBoundsException
  override def head = throw new ArrayIndexOutOfBoundsException
  override def tail = throw new ArrayIndexOutOfBoundsException
  override def <+>[B](other:SList[B]):SList[B] = other
  override val size:Int = 0
  override def removeNthNodeFromEnd(n: Int): SList[Nothing] = throw new IllegalAccessException()
  override def map[B](f: Nothing => B): SList[B] = Nill
  override def flatMap[B](f: Nothing => SList[B]): SList[B] = Nill
  override val isPalindrome:Boolean = false
  override def reverse: SList[Nothing] = Nill
  override def foldLeft[B >: Nothing](z: B)(f: (Nothing, B) => B): B = z
  override def foldRight[B >: Nothing](z: B)(f: (Nothing, B) => B): B = z
}

object SList {
  def apply[A](contents: A*): SList[A] = contents.foldLeft(SList.empty[A])((x, y) => x <+ y)
  def empty[A]: SList[A] = Nill
  def areSame[A](thiz:SList[A],that:SList[A]):Boolean = {
    if((thiz == Nill && that != Nill) || (that == Nill && thiz != Nill)) false
    else if(thiz == Nill && that == Nill) true
    else thiz.head == that.head && areSame(thiz.tail,that.tail)
  }
}