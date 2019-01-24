package org.scala.fds

trait KList[+A] {
  def tail:KList[A]
  def reverse:KList[A]
  def length: Int
  def removeNthNodeFromEndOfList(n:Int):KList[A] = this match {
    case ls if ls.length == n => tail
    case Cons(head,rest) if rest.length < n => throw new IllegalArgumentException
    case Cons(head,rest) if rest.length > n => Cons(head,rest.removeNthNodeFromEndOfList(n))
    case Cons(head,rest) if rest.length == n => Cons(head,rest.tail)
  }
  def rotate(n:Int):KList[A]
  def rotate: KList[A]
  def deDup: KList[A]
  def middle:A
  def head:A
  def flatMap[B](f: A => KList[B]):KList[B] = this match {
    case Nil => Nil
    case Cons(head, rest) => KList.append(f(head), rest.flatMap(f))
  }
  def isPallindrome: Boolean

  def ===(that: scala.Any): Boolean = that match {
    case Nil => this == Nil
    case Cons(head,tail) => head == this.head && tail == this.tail
    case _ => false
  }
}

abstract class Foldable[F[_]](xs:F) {
  def head[A]:A
  def tail[A]:F[A]
  def foldLeft[A,B](z: A)(f: (A,B) => B):B
}


case class Cons[A](head:A,tail:KList[A]) extends KList[A] {
  override def reverse: KList[A] = KList.append(tail.reverse,head)
  override def toString: String = s"$head ${tail.toString}"
  override def length:Int = 1 + tail.length
  override def rotate = KList.append(tail,head)
  override def rotate(n:Int) = if(n == 0) this else this.rotate.rotate(n-1)
  override def deDup: KList[A] = this match {
    case Cons(a,Cons(b,rest)) if a == b => Cons(a,rest).deDup
    case Cons(a,rest) => Cons(a,rest.deDup)
  }

  override def middle: A = {
    def _m(tur:KList[A], har: KList[A]):A = har match {
      case Nil => tur.head
      case Cons(a,Nil) => tur.head
      case _ => _m(tur.tail,har.tail.tail)
    }
    _m(this,this)
  }

  override def isPallindrome: Boolean = this === reverse

}
case object Nil extends KList[Nothing] {
  override def reverse = Nil
  override def toString:String = ""
  override def length:Int = 0
  override def tail:KList[Nothing] = throw new IllegalArgumentException
  override def rotate(n:Int):KList[Nothing] = Nil
  override def rotate:KList[Nothing] = Nil
  override def deDup: KList[Nothing] = Nil
  override def middle : Nothing = throw new IllegalArgumentException
  override def head: Nothing = throw new IllegalArgumentException

  override def isPallindrome: Boolean =  true
}
object KList {
  def apply[A](as: A*):KList[A] = app(as.seq)

  private def app[A](a:Seq[A]): KList[A] = a.headOption match {
    case None => Nil
    case Some(head) => Cons(head,app(a.tail))
  }

  def append[A](list:KList[A], item:A):KList[A] = list match {
    case Nil => Cons(item,Nil)
    case Cons(head,tail) => Cons(head,append(tail,item))
  }

  def append[A](thiz: KList[A], that: KList[A]):KList[A] = thiz match {
    case Cons(a,Nil) => Cons(a,that)
    case Cons(head,tail) => Cons(head,append(tail,that))
  }

  def main(args: Array[String]): Unit = {
    println(KList(2,1,1,3,1,1,2).isPallindrome)

    def conv[A](a: List[A]):Foldable[List[A]] = new Foldable(a) {
      def head: A = a.head
      def tail: List[A] = a.tail
      def foldLeft[A, B](z: A)(f: (A, B) => B): B = ???
    }
  }
}