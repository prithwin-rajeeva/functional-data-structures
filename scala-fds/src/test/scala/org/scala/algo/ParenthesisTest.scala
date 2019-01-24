package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class ParenthesisTest extends FunSpec with Matchers {
  describe("Parenthesis") {
    it("should work") {
      Permutations.balanceParenteses(5).map(Parenthesis.isValid(_) should be(true))
      Permutations.bizzaroBalanceParenteses(5).map(Parenthesis.isValid(_) should be(false))
    }

    it("should create all parenthese") {
      println(Parenthesis.generate(3))
    }
  }
}
