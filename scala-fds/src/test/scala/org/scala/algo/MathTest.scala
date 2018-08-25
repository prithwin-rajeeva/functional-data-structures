package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class MathTest extends FunSpec with Matchers {
  describe("Factorial"){
    it("should be correct") {
      Math.!(5) should be(120)
    }
  }

  describe("power"){
    it("should be correct") {
      Math.pow(3,3) should be(27)
      Math.pow(2,3) should be(8)
      Math.pow(3,2) should be(9)
      Math.pow(2,2) should be(4)
    }
  }

}
