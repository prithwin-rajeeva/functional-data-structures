package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class PermutationsTest extends FunSpec with Matchers {
  describe("permutations") {
    it("should permute Strings") {
      //its very easy to just fire this and test it manually
      Permutations.permute(List(1,2,3,4)).foreach(println)
    }
  }

  describe("parentheses generator") {
    it("should generate all combinations of tree parens") {
      Permutations.balanceParenteses(2).foreach(println)
    }
  }
}
