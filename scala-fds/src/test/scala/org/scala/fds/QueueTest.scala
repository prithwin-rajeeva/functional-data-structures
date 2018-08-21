package org.scala.fds

import org.scalatest.{FunSpec, Matchers}

class QueueTest extends FunSpec with Matchers {
  describe("Queue") {
    it("should be able to instantiate and add some elemements to it") {
      val singleElementStack = QueueAdt.empty[Int].put(1)
      singleElementStack.peek should be(1)
    }

    it("should be able to add multiple elements onto this stack") {
      val multiElementStack = QueueAdt.empty[Int].put(1).put(2).put(3).put(4)
      multiElementStack.peek should be(1)
    }

    it("should be able to take elements from it") {
      val multiElementStack = QueueAdt.empty[Int].put(1).put(2).put(3).put(4)
      multiElementStack.take.peek should be(2)
      multiElementStack.take.take.peek should be(3)
    }
  }
}
