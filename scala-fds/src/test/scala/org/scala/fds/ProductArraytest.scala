package org.scala.fds

import org.scala.algo.ProductArray
import org.scalatest.{FunSpec, Matchers}

class ProductArraytest extends FunSpec with Matchers {
  describe("productArray") {
    it("should create the product array") {
      println(ProductArray.getProductSeq(List(1,2,3,4)))
      println(ProductArray.getProductEff(List(1,2,3,4)))
    }
  }

}
