package org.scala.fds

trait QueueAdt[+T] {
  def put[U >: T](in:U):QueueAdt[U]
  def take:QueueAdt[T]
  def peek:T
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
}

case object EmptyQueue extends QueueAdt[Nothing]{
  override def put[U >: Nothing](in: U): QueueAdt[U] = NonEmptyQueue(in,EmptyQueue)
  override def take: QueueAdt[Nothing] = throw new IllegalAccessException("can't take anything from an empty" +
    " Queue")
  override def peek: Nothing = throw new IllegalAccessException("can't peek anything in a empty queue")
}

object QueueAdt {
  def empty[T]:QueueAdt[T] = EmptyQueue
}

