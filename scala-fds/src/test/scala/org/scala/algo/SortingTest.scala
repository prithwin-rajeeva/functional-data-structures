package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class SortingTest extends FunSpec with Matchers {
  describe("BubbleSort") {
    it("should sort the elements") {
      println(Sorting.selectionSort(List(-5,5,23,0,-34,67)))
    }
  }

}
