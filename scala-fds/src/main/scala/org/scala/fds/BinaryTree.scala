package org.scala.fds

/**
  * simply binary tree that lets you some of the operations on a binary tree
  * @tparam A
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
  def spiralOrderString:String = ???
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
        val node = pq.peek
        node match {
          case Term => _loPrint(pq.take,res.:+(node.valueString))
          case NonEmpty(v,l,r) => _loPrint(pq.take.put(l).put(r),res.:+(node.valueString))
        }
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
}

object Node {
  def apply[A](value:A): BinaryTree[A] = NonEmpty(value,Term,Term)
  def apply[A](value:A,left: BinaryTree[A],right: BinaryTree[A]):BinaryTree[A] = NonEmpty(value,left,right)
}
