package org.scala.fds

import org.scalatest.{FunSpec, Matchers}

class StackTest extends FunSpec with Matchers{
  describe("StackAdt") {
    it("should be able to push elements onto empty stacks") {
      val singleElementStack = StackAdt.empty.push(1)
      singleElementStack.peek should be(1)
    }

    it("should be able to add multiple elements onto the stack") {
      val multiElementStack = StackAdt.empty.push(1).push(2).push(3).push(4)
      multiElementStack.peek should be(4)
      multiElementStack.size should be(4)
    }

    it("popping should result in decrement of size") {
      val multiElementStack = StackAdt.empty.push(1).push(2).push(3).push(4)
      multiElementStack.peek should be(4)
      multiElementStack.size should be(4)
      multiElementStack.pop.size should be (3)
      multiElementStack.pop.peek should be (3)
      multiElementStack.pop.pop.pop.pop should matchPattern{case EmptyStack =>}

    }
  }
}
