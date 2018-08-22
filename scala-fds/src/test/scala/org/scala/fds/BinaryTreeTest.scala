package org.scala.fds

import org.scalatest.{FunSpec, Matchers}

class BinaryTreeTest extends FunSpec with Matchers {
  describe("BinaryTree") {
    it("should be able to create unit BinaryTree") {
      val binaryTree: BinaryTree[Int] = Node(1)
      binaryTree.value should be(1)
      binaryTree.left should be(Term)
      binaryTree.right should be(Term)
    }

    it("should be able to make complicated trees") {
      val binaryTree: BinaryTree[Int] = Node(1,Node(2,Node(4),Node(5)),Node(3,Node(6),Node(7)))
      println(binaryTree)
      binaryTree.left.left.value should be(4)
    }

    /**
      *       1
      *     /  \
      *    2    3
      *  /  \  / \
      *  4  5  6  7
      *          / \
      *         8  9
      */
    it("it should print stuff for manual verification") {
      val binaryTree = Node(1,Node(2,Node(4),Node(5)),Node(3,Node(6),Node(7,Node(8),Node(9))))
      val binaryTreeDup = Node(1,Node(2,Node(4),Node(5)),Node(3,Node(6),Node(7,Node(8),Node(9))))
      val randTree =  Node(1,Node(2,Node(4),Node(5)),Node(3,Node(6),Node(7)))
      println(binaryTree.inOrderString)
      println(binaryTree.preOrderString)
      println(binaryTree.postOrderString)
      binaryTree.height should be(4)
      binaryTree.size should be(9)
      println("printing in level order")
      println(binaryTree.levelOrderString)
      println("printing in spiral order")
      println(binaryTree.spiralOrderString)
      binaryTree == binaryTreeDup should be(true)
      binaryTree == randTree should be(false)
      println(binaryTree.mirror.levelOrderString)
      binaryTree.countLeaves should be(5)
      binaryTreeDup.countLeaves should be(5)
      randTree.countLeaves should be(4)
    }
  }
}
