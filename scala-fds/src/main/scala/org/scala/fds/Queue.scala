package org.scala.fds

trait QueueAdt[+T] {
  self =>
  def put[U >: T](in:U):QueueAdt[U]
  def take:QueueAdt[T]
  def peek:T
  def isEmpty:Boolean
  def length:Int
  def toList:List[T] = self match {
    case EmptyQueue => scala.Nil
    case NonEmptyQueue(a,b) => b.toList.:+(a)
  }
}

case class NonEmptyQueue[+T](head:T,rest:QueueAdt[T]) extends QueueAdt[T] {
  override def put[U >: T](in: U): QueueAdt[U] = NonEmptyQueue(in,this)
  override def take: QueueAdt[T] = this match {
    case NonEmptyQueue(hd,EmptyQueue) => EmptyQueue
    case NonEmptyQueue(hd,tl) => NonEmptyQueue(hd,tl.take)
  }
  override def peek: T = this match {
    case NonEmptyQueue(_,EmptyQueue) => head
    case NonEmptyQueue(_,tl) => tl.peek
  }
  override val isEmpty: Boolean = false
  override def length:Int = 1 + rest.length
}

case object EmptyQueue extends QueueAdt[Nothing]{
  override def put[U >: Nothing](in: U): QueueAdt[U] = NonEmptyQueue(in,EmptyQueue)
  override def take: QueueAdt[Nothing] = throw new IllegalAccessException("can't take anything from an empty" +
    " Queue")
  override def peek: Nothing = throw new IllegalAccessException("can't peek anything in a empty queue")
  override val isEmpty: Boolean = true
  override val length : Int = 0
}

object QueueAdt {
  def empty[T]:QueueAdt[T] = EmptyQueue
}


