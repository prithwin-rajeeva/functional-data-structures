package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class LocalMaximaTest extends FunSpec with Matchers {
  describe("LocalMaxima") {
    it("should find the local maxima") {
      println(BigDecimal(LocalMaxima.findMaxValue(
        x => (x*x*x) + (3 * x *x) - (2 * x) + 1,
        -4,
        2
      )))

      println( -1 - math.sqrt(15)/3)
    }
  }
}
