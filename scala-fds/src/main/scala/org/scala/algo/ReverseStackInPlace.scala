package org.scala.algo

import org.scala.fds.{EmptyStack, NonEmptyStack, StackAdt}

object ReverseStackInPlace {
  def reverseStack[T](stack:StackAdt[T]):StackAdt[T] = stack match {
    case EmptyStack => EmptyStack
    case x: NonEmptyStack[T] => NonEmptyStack.pushAll(EmptyStack,x)
  }

  def main(args: Array[String]): Unit = {
    val stack = EmptyStack.push(1).push(2).push(3).push(4)
    println(stack)
    println(reverseStack(stack))
  }
}
