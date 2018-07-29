package org.scala.fds

import org.scalatest.FunSpec
import org.scala.fds.SList._

class ListTest extends FunSpec{
  describe("List") {
    it("empty List should say its empty") {
      assert(SList().isEmpty)
    }

    it("no empty lists shouldn't say its empty") {
      assert(SList(1,2,3,4,5).isNonEmpty)
    }

    it("empty list should contains nothing" ) {
      assert(!SList().contains(1))
    }

    it("should contain the single element just added") {
      assert(SList(1).contains(1))
    }

    it("should not lie about its contents") {
      assert(!SList(1).contains(5))
    }

    it("freshly appended values should be accounted for") {
      assert(SList() <+ 1 contains 1 )
      assert(SList() <+ 1 <+ 2 <+ 3  contains 3 )
      assert(! (SList() <+ 1 <+ 2 <+ 3  contains 4 ))
    }

    it("should contain all the elements that were just added") {
      assert(SList(1,2,3).contains(1))
      assert(SList(1,2,3).contains(2))
      assert(SList(1,2,3).contains(3))
    }

    it("should return its indices correctly") {
      val myList = SList(1,2,3)
      assert(myList(0) == 1)
      assert(myList(1) == 2)
      assert(myList(2) == 3)
    }

    it("should throw ArrayIndexOutOfBoundsException if negetive index") {
      val myList = SList(1,2,3)
      assertThrows[ArrayIndexOutOfBoundsException](myList(-1))
    }

    it("should throw ArrayIndexOutOfBoundsException if exceeds elements") {
      val myList = SList(1,2,3)
      assertThrows[ArrayIndexOutOfBoundsException](myList(4))
    }

    it("appendedList should contain all the elements") {
      val result = SList(1,2,3) <+> SList(4,5,6)
      assert(result.contains(1))
      assert(result.contains(2))
      assert(result.contains(3))
      assert(result.contains(4))
      assert(result.contains(5))
      assert(result.contains(6))
      assert(!result.contains(19))
    }

    it("should tell the size of a 3 item list correctly") {
      assert(SList(1,2,3).size == 3)
    }

    it("should return 0 for emptly lists") {
      assert(SList().size == 0)
    }

    it("should return the list size in case of joined list") {
      val a = SList(1,2,3)
      val b = SList(1,2,3)
      assert((a <+> b).size == a.size + b.size)
    }

    it("equal lists should be identified as equal") {
      assert(SList(1,2,3,4) == SList(1,2,3,4))
    }
  }
}
