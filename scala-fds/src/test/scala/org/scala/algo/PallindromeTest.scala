package org.scala.algo

import org.scalatest.{FunSpec, Matchers}

class PallindromeTest extends FunSpec with Matchers {
  describe("Pallindromer") {
    it("should find palindromes") {
      Pallindrome.isPalindrome("ABBA") should be(true)
      Pallindrome.isPalindrome("MALAYALAM") should be(true)
      Pallindrome.isPalindrome("A") should be(true)
      Pallindrome.isPalindrome("AAAAA") should be(true)
      Pallindrome.isPalindrome("AAAAAA") should be(true)
    }
    it("should find imposters") {
      Pallindrome.isPalindrome("Prithwin") should be(false)
      Pallindrome.isPalindrome("Thrishya") should be(false)
      Pallindrome.isPalindrome("Dam") should be(false)
    }
  }
}
