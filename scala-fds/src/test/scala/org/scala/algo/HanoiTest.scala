package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class HanoiTest extends FunSpec with Matchers {
  describe("towers") {
    it("should list all the moves") {
      TowerOfHanoi.printMoves(3)
    }
  }
}
