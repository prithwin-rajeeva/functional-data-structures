package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class RotateSeqTest extends FunSpec with Matchers {
  describe("rotator") {
    it("should rotate a list") {
      RotateSeq.rotateRight(List(1,2,3,4,5),1) should be(List(2,3,4,5,1))
      RotateSeq.rotateRight(List(1,2,3,4,5),2) should be(List(3,4,5,1,2))
      RotateSeq.rotateRight(List(1,2,3,4,5),3) should be(List(4,5,1,2,3))
      RotateSeq.rotateRight(List(1,2,3,4,5),4) should be(List(5,1,2,3,4))
      RotateSeq.rotateRight(List(1,2,3,4,5),5) should be(List(1,2,3,4,5))
      RotateSeq.rotateRight(List(1,2,3,4,5),6) should be(List(2,3,4,5,1))
    }

    it("should rotate a list towards the left") {
      RotateSeq.rotateLeft(List(1,2,3,4,5),1) should be(List(5,1,2,3,4))
      RotateSeq.rotateLeft(List(1,2,3,4,5),2) should be(List(4,5,1,2,3))
      RotateSeq.rotateLeft(List(1,2,3,4,5),3) should be(List(3,4,5,1,2))
      RotateSeq.rotateLeft(List(1,2,3,4,5),4) should be(List(2,3,4,5,1))
      RotateSeq.rotateLeft(List(1,2,3,4,5),5) should be(List(1,2,3,4,5))
      RotateSeq.rotateLeft(List(1,2,3,4,5),6) should be(List(5,1,2,3,4))
    }
  }
}
