package org.scala.fds

trait StackAdt[+T] {
  def push[U >: T](in:U):StackAdt[U]
  def pop:StackAdt[T]
  def peek:T
  def size:Int
  def isEmpty:Boolean
  def reverse: StackAdt[T]
}

case class NonEmptyStack[+T](top:T,rest:StackAdt[T]) extends StackAdt[T] {
  override def push[U >: T](in: U): StackAdt[U] = NonEmptyStack(in,this)
  override def pop: StackAdt[T] = rest
  override def peek: T = top
  override def size: Int = 1 + rest.size
  override def isEmpty: Boolean = false

  def reverse: StackAdt[T] = {
    val reversedRest = rest.reverse
    NonEmptyStack.pushAll(EmptyStack.push(top),reversedRest)
  }
}

object NonEmptyStack {
  def pushAll[T](some:StackAdt[T],other: StackAdt[T]):StackAdt[T] = {
    if(other.isEmpty)
      some
    else
      pushAll(some.push(other.peek),other.pop)
  }
}

case object EmptyStack extends StackAdt[Nothing] {
  override def push[U >: Nothing](in: U): StackAdt[U] = NonEmptyStack(in,EmptyStack)
  override def pop: StackAdt[Nothing] = throw new IllegalAccessException("can't pop an empty stack")
  override def peek: Nothing = throw new IllegalArgumentException("can't peek and empty stack")
  override def size: Int = 0
  override def isEmpty: Boolean = true
  def reverse: StackAdt[Nothing] = EmptyStack
}

object StackAdt {
  def empty[T]:StackAdt[T] = EmptyStack
}