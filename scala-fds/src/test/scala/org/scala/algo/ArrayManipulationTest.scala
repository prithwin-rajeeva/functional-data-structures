package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class ArrayManipulationTest extends FunSpec with Matchers {
  describe("Array manipulator") {
    it("should be able to reverse an array") {
       ArrayManipulation.reverse(Array(1,2,3,4,5)) should be(Array(5,4,3,2,1))
       ArrayManipulation.reverse(Array(1,2,3,4)) should be(Array(4,3,2,1))
    }

    it("should rotate a matrix") {

      val a = Array(
        Array(1, 2, 3, 4),
        Array(1, 2, 3, 4),
        Array(1, 2, 3, 4),
        Array(1, 2, 3, 4)
      )
      a.foreach(array => {
        array.foreach(item => print(s" $item "))
        println
      })
      println()
      println()
      println()
      val x = ArrayManipulation.rotateMatrixLeft(Array(
        Array(1, 2, 3, 4),
        Array(1, 2, 3, 4),
        Array(1, 2, 3, 4),
        Array(1, 2, 3, 4)
      ))
      x.foreach(array => {
        array.foreach(item => print(s" $item "))
        println
      })
      println()
      println()
      println()
      val y = ArrayManipulation.rotateMatrixRight(ArrayManipulation.rotateMatrixRight(Array(
        Array(1, 2, 3, 4),
        Array(1, 2, 3, 4),
        Array(1, 2, 3, 4),
        Array(1, 2, 3, 4)
      )))
      y.foreach(array => {
        array.foreach(item => print(s" $item "))
        println
      })
    }
  }
}
