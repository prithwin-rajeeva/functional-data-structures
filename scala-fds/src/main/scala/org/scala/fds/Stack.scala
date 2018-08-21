package org.scala.fds

trait StackAdt[+T] {
  def push[U >: T](in:U):StackAdt[U]
  def pop:StackAdt[T]
  def peek:T
  def size:Int
}

case class NonEmptyStack[+T](top:T,rest:StackAdt[T]) extends StackAdt[T] {
  override def push[U >: T](in: U): StackAdt[U] = NonEmptyStack(in,this)
  override def pop: StackAdt[T] = rest
  override def peek: T = top
  override def size: Int = 1 + rest.size
}

case object EmptyStack extends StackAdt[Nothing] {
  override def push[U >: Nothing](in: U): StackAdt[U] = NonEmptyStack(in,EmptyStack)
  override def pop: StackAdt[Nothing] = throw new IllegalAccessException("can't pop an empty stack")
  override def peek: Nothing = throw new IllegalArgumentException("can't peek and empty stack")
  override def size: Int = 0
}

object StackAdt {
  def empty[T]:StackAdt[T] = EmptyStack
}