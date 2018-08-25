package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class SubsetSumProblemTest extends FunSpec with Matchers {
  describe("Subset sum Problem") {
    it("should be solved successfully") {
      SubsetSumProblem.subsetSumExists(Array(3, 34, 4, 12, 5, 2),9) should be(true)
      SubsetSumProblem.subsetSumExists(Array(3, 34, 4, 12, 5, 2),99) should be(false)
    }
  }
}
