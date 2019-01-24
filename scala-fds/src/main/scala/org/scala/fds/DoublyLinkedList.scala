package org.scala.fds

trait DoublyLinkedList[+A] {
  def reverse: DoublyLinkedList[A]
  def flatten: DoublyLinkedList[A]
}
case class NonEmptyDLL[A](prev:DoublyLinkedList[A],head:A,next:DoublyLinkedList[A]) extends DoublyLinkedList[A] {
  def reverse: DoublyLinkedList[A] = NonEmptyDLL(next.reverse,head,prev.reverse)

  def flatten: DoublyLinkedList[A] = ???
}
case class MultiLevelDLL[A](prev:DoublyLinkedList[A],head:DoublyLinkedList[A],child:DoublyLinkedList[A],next:DoublyLinkedList[A]) extends DoublyLinkedList[A] {
  def reverse: DoublyLinkedList[A] = throw new Exception

  def flatten: DoublyLinkedList[A] = ???
//  def flatten: DoublyLinkedList[A] = {
//    def _f(q: QueueAdt[DoublyLinkedList[A]],acc:DoublyLinkedList[A]):DoublyLinkedList[A] = {
//      if(q.isEmpty) acc
//      else {
//        val qTop = q.peek
//        val remQ = q.take
//        qTop match {
//          case MultiLevelDLL(p,v,`Nool`,n) => _f(q,MultiLevelDLL.append())
//        }
//      }
//    }
//    _f(QueueAdt.empty.put(this),Nool)
//  }
}

object MultiLevelDLL {
//  def append[A](a:MultiLevelDLL[A],b:A)
}

case object Nool extends DoublyLinkedList[Nothing] {
  def reverse : DoublyLinkedList[Nothing] = Nool

  def flatten: DoublyLinkedList[Nothing] = Nool
}

object DoublyLinkedList {
}