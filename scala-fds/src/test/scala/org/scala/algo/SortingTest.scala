package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class SortingTest extends FunSpec with Matchers {
  describe("Sorter") {
    it("should sort the elements") {
      Sorting.selectionSort(List(-5,5,23,0,-34,67)) should be(List(-34, -5, 0, 5, 23, 67))
      Sorting.mergeSort(List(-5,5,23,0,-34,67)) should be (List(-34, -5, 0, 5, 23, 67))
    }
  }

}
