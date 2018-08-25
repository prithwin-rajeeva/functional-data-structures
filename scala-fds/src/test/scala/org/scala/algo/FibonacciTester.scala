package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class FibonacciTester extends FunSpec with Matchers {
  describe("Fibonacci"){
    it("should get the nth fibonacin number correctly") {
      Fibonacci.get(1) should be(1)
      Fibonacci.get(2) should be(1)
      Fibonacci.get(3) should be(2)
      Fibonacci.get(4) should be(3)
      Fibonacci.get(5) should be(5)
    }
    it("should produce list of fibonacci numbers correctly") {
      Fibonacci.list(5) should be(List(1,1,2,3,5))
    }
  }
}
