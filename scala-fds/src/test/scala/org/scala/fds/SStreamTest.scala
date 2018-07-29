package org.scala.fds

import org.scalatest.FunSpec

class SStreamTest extends FunSpec{
  describe("SStream") {
    it("should create a stream") {
     SStream.cons(getNum(1),SStream.cons(getNum(2),SStream.cons(getNum(2),SStream.empty)))
    }

    it("it should compute the thing") {
      SStream.range(1,1000000).filter(_ % 2 == 0).foreach(println)
    }
  }

  def getNum(i: Int):Int = {
    println(s"computing $i")
    i
  }
}
