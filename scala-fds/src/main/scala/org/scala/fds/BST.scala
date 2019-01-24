package org.scala.fds

trait BST[+A] {
  def toString: String
}

case class TreeNodeX[+A](root: A, left: BST[A], right: BST[A]) extends BST[A] {
  override def toString: String = s" ${left.toString} $root  ${right.toString}"
}

case object leaf extends BST[Nothing] {
  override def toString: String = ""
}

object BST {
  def add[A](tree: BST[A], item: A)(implicit conv: A => Ordered[A]): BST[A] = tree match {
    case `leaf` => TreeNodeX(item, `leaf`, `leaf`)
    case TreeNodeX(a, left, right) if a > item => TreeNodeX(a, add(left, item), right)
    case TreeNodeX(a, left, right) if a < item => TreeNodeX(a, left, add(right, item))
    case a => a
  }

  def remove[A](tree: BST[A], item: A)(implicit conv: A => Ordered[A]): BST[A] = tree match {
    case TreeNodeX(a, left, right) if item < a => TreeNodeX(a, remove(left, item), right)
    case TreeNodeX(a, left, right) if item > a => TreeNodeX(a, left, remove(right, item))
    case TreeNodeX(a, left, right) if a == item => TreeNodeX(inOrderSuccessor(right), left, trimInorderSuccessor(right))
    case _ => throw new IllegalStateException
  }

  private def inOrderSuccessor[A](tree: BST[A]): A = tree match {
    case TreeNodeX(a, `leaf`, _) => a
    case TreeNodeX(a, left, _) => inOrderSuccessor(left)
  }

  private def trimInorderSuccessor[A](tree: BST[A]): BST[A] = tree match {
    case TreeNodeX(a, `leaf`, right) => right
    case TreeNodeX(a, left, right) => TreeNodeX(a, trimInorderSuccessor(left), right)
  }

  def main(args: Array[String]): Unit = {
    val tree2 = TreeNodeX(4,TreeNodeX(2,TreeNodeX(1,leaf,leaf),TreeNodeX(3,leaf,leaf)),
      TreeNodeX(6,TreeNodeX(5,leaf,leaf),TreeNodeX(7,leaf,leaf))
    )
    println(split(tree2,6))
  }

  def split[A](tree:BST[A],limit:A)(implicit conv: A => Ordered[A]):(BST[A],BST[A]) = tree match {
    case TreeNodeX(root,left,right) if limit == root => (TreeNodeX(root,left,leaf),right)
    case TreeNodeX(root,left,right) if limit < root => (split(left,limit)._1,pruneLeft(tree,limit))
    case TreeNodeX(root,left,right) if limit > root => (pruneRight(tree,limit),split(right,limit)._2)
    case _ => throw new IllegalArgumentException
  }

  def pruneLeft[A](tree:BST[A],limit:A)(implicit conv: A => Ordered[A]):BST[A] = tree match {
    case TreeNodeX(`limit`,left,right) => right
    case TreeNodeX(a,left,right) => TreeNodeX(a,pruneLeft(left,limit),right)
    case _ => throw new IllegalAccessException
  }

  def pruneRight[A](tree:BST[A],limit:A)(implicit conv: A => Ordered[A]):BST[A] = tree match {
    case TreeNodeX(`limit`,left,right) => TreeNodeX(limit,left,leaf)
    case TreeNodeX(a,left,right) => TreeNodeX(a,left,pruneRight(right,limit))
    case _ => throw new IllegalAccessException
  }
}



