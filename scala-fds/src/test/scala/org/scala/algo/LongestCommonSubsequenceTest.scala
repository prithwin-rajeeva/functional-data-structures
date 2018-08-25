package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class LongestCommonSubsequenceTest extends FunSpec with Matchers {
  describe("LongestCommonSubsequenceTest") {
    it("should find the LCS") {
      LongestCommonSubsequence.measure("AGGTAB","GXTXAYB") should be(4)
      LongestCommonSubsequence.measureDp("AGGTAB","GXTXAYB") should be(4)
      LongestCommonSubsequence.measure("ABCDGHLQRtyu2","AEDPHRtyu1") should be(7)
      LongestCommonSubsequence.measureDp("ABCDGHLQRtyu5","AEDPHRtyux6") should be(7)
    }
  }
}
