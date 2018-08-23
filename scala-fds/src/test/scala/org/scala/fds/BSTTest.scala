package org.scala.fds

import org.scalatest.{FunSpec, Matchers}

class BSTTest extends FunSpec with Matchers{
  describe("BST") {
    it("should work for simple insertion and lookup operations") {
      val bst = TreeSortedMap.empty[String,String] +("Prithwin","Thrishya") + ("Ashwin","srividya")
      bst("Prithwin") should be(Some("Thrishya"))
      bst("Prithwi") should be(None)
      bst("Ashwin") should be(Some("srividya"))
    }

    it("should be able to have one of its nodes deleted") {
      val bst = TreeSortedMap.empty[String,Int] + ("T",1) + ("Z",2) + ("C",3) + ("A",5)
      bst("C") should be(Some(3))
      val removedBst = bst - "C"
      removedBst("C") should be (None)
      bst("T") should be(Some(1))
      bst("Z") should be(Some(2))
      bst("A") should be(Some(5))
    }
  }
}
