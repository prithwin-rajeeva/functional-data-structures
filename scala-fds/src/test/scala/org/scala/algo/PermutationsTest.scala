package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class PermutationsTest extends FunSpec with Matchers {
  describe("permutations") {
    it("should permute Strings") {
      //its very easy to just fire this and test it manually
      Permutations.permute(List(1,2,3,4)).foreach(println)
    }
  }
}
