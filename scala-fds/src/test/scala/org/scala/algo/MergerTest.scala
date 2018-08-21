package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class MergerTest extends FunSpec with Matchers {
  describe("Merger") {
    it("should merge alternate number correctly") {
      Merger.merge(List(1,3,5,7),List(2,4,6,8)) should be(List(1,2,3,4,5,6,7,8))
    }

    it("should manage merging with blank lists") {
      Merger.merge(List(1,4,6,67,89),List.empty) should be(List(1,4,6,67,89))
    }
  }
}
