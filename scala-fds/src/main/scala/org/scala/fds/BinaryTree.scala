package org.scala.fds
import scala.collection.immutable

/**
  * simply binary tree that lets you some of the operations on a binary tree
  * @tparam A any type
  */
trait BinaryTree[+A] {
  def left:BinaryTree[A]
  def right:BinaryTree[A]
  def value : A
  def preOrderString:String
  def inOrderString:String
  def postOrderString:String
  def height:Int
  def size:Int
  def levelOrderString:String
  def valueString:String
  def tequals(obj: Any): Boolean
  def mirror:BinaryTree[A]
  def countLeaves:Int
  def spiralOrderString:String
  def isBst:Boolean
  def allPaths:List[List[A]] = {
    def _ap(node:BinaryTree[A]):List[List[A]] = node match {
      case NonEmpty(v,Term,Term) => List(List(v))
      case NonEmpty(v,l,r) =>  (_ap(l) ++ _ap(r)).map(path => path.+:(v))
    }
    _ap(this)
  }

}

case class NonEmpty[+A](value:A,left: BinaryTree[A],right: BinaryTree[A]) extends BinaryTree[A]{
  override def toString: String = s"($value (${left.toString} , ${right.toString})"
  override def preOrderString: String = toString
  override def inOrderString:String = s"(${left.toString}, ($value), ${right.toString})"
  override def postOrderString:String = s"((${left.toString} , ${right.toString} $value)"
  override def height: Int = 1 + math.max(left.height,right.height)
  override def size: Int = 1+ left.size + right.size
  override def levelOrderString: String = {
    def _loPrint(pq:QueueAdt[BinaryTree[A]],res:List[String]):List[String] = {
      if(pq.isEmpty) res
      else {
        val line = pq.toList
        val level = line.map(_.valueString)
        val nextQueue = line
          .foldLeft(QueueAdt.empty[BinaryTree[A]])((a, b) => if(b != Term) a.put(b.left).put(b.right) else a)
        _loPrint(nextQueue,res ++ level)
      }
    }

    val printQueue = QueueAdt.empty[BinaryTree[A]]
    _loPrint(printQueue.put(this),List.empty[String]).filter(_!="").toString

  }
  override def valueString: String = value.toString

  override def tequals(obj: Any): Boolean = obj match {
    case NonEmpty(v,l,r) => v == value && left == l && right == l
    case _ => false
  }

  def mirror:BinaryTree[A] = {
    Node(value,right.mirror,left.mirror)
  }

  override def countLeaves: Int = this match {
    case NonEmpty(v,Term,Term) => 1
    case NonEmpty(v,l,Term) => l.countLeaves
    case NonEmpty(v,Term,r) => r.countLeaves
    case NonEmpty(v,l,r) =>  l.countLeaves + r.countLeaves
  }
  override def spiralOrderString: String = {
    def _loPrint(pq:QueueAdt[BinaryTree[A]],res:List[String],dir:Boolean):List[String] = {
      if(pq.isEmpty) res
      else {
        val line = pq.toList
        val level = line.map(_.valueString)
        val nextQueue = line
          .foldLeft(QueueAdt.empty[BinaryTree[A]])((a, b) => if(b != Term) a.put(b.left).put(b.right) else a)
       _loPrint(nextQueue,res ++ (if(dir) level else level.reverse),!dir)
      }
    }

    val printQueue = QueueAdt.empty[BinaryTree[A]]
    _loPrint(printQueue.put(this),List.empty[String],true).filter(_!="").toString

  }
  override def isBst: Boolean = false
}

case object Term extends BinaryTree[Nothing] {
  override def left: BinaryTree[Nothing] = throw new IllegalAccessException()
  override def right: BinaryTree[Nothing] = throw new IllegalAccessException()
  override def value: Nothing = throw new IllegalArgumentException
  override val toString: String = "X"
  override val preOrderString:String = "X"
  override val inOrderString:String = "X"
  override val postOrderString:String = "X"
  override val height: Int = 0
  override val size: Int = 0
  override val levelOrderString: String = "X"
  override def valueString: String = ""
  override def tequals(obj: scala.Any): Boolean = obj match {
    case Term => true
    case _ => false
  }
  override def mirror:BinaryTree[Nothing] = Term
  override def countLeaves: Int = throw new IllegalAccessException()
  override def spiralOrderString: String = ""
  override val isBst: Boolean = true
}

object Node {
  def apply[A](value:A): BinaryTree[A] = NonEmpty(value,Term,Term)
  def apply[A](value:A,left: BinaryTree[A],right: BinaryTree[A]):BinaryTree[A] = NonEmpty(value,left,right)
}
