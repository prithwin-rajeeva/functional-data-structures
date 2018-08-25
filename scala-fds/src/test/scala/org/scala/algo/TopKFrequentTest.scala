package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class TopKFrequentTest extends FunSpec with Matchers {
  describe("TopKFrequent") {
    it("should find the top k frequent") {
      println(TopKFrequentWords.topKFrequent(Array("i", "love", "leetcode", "i", "love", "coding"),2))
    }
  }
}
