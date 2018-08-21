package org.scala.fds

import org.scala.fds.SStream.cons

trait SStream[+A] {
  def head:A
  def tail:SStream[A]
  def isEmpty:Boolean
  def foreach[B](f: A => B):Unit = {
    f(head)
    tail.foreach(f)
  }

  def filter(p: A => Boolean):SStream[A] = {
    if(isEmpty) this
    else if(p(head)) SStream.cons(head,tail.filter(p))
    else
      tail.filter(p)

  }

  def take(num:Int):SStream[A] = {
    if(num <= 0) SStream.empty
    else cons(head,tail.take(num-1))
  }

  def toList:List[A] = {
    if(tail == SStream.empty)
      head :: Nil
    else
      head :: tail.toList
  }
}
object SStream {
  /*
    streams require this kind of constructor that builds and implementation of a trait because case classes cannot take
    a thunk as a parameter
   */
  def cons[A](hd: A, tl: => SStream[A]): SStream[A] = new SStream[A] {
    override def head:A = hd
    override def tail = tl
    def isEmpty = false

    override def toString:String = {
      s"SStream($hd , ?)"
    }
  }

  def from(start:Int):SStream[Int] = {
    cons(start,SStream.from(start+1))
  }

  def range(from:Int ,to:Int):SStream[Int] = {
    if(from >= to) empty
    else
      cons(getNum(from),range(from+1,to))
  }

  def empty = new SStream[Nothing] {
    override def head = throw new NoSuchElementException
    override def tail = throw new NoSuchElementException
    override def isEmpty: Boolean = true
  }

  def apply[A](items:Seq[A]):SStream[A] =
    if (items.isEmpty)
      SStream.empty
    else
      cons(items.head, SStream(items.tail))

  def sieve(target:SStream[Int]):SStream[Int] = {
    cons(target.head,sieve(target.filter(_ % target.head !=0)))
  }

  def getNum(i: Int):Int = {
    println(s"computing $i")
    i
  }
}