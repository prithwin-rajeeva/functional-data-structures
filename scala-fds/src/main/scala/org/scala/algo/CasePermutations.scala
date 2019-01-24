package org.scala.algo

import scala.collection.immutable

object CasePermutations {
  def permute(originalString: String): List[String] = {
    (0 until math.pow(2, originalString.length).toInt)
      .toList
      .map(_.toBinaryString.reverse.padTo(originalString.length, "0").reverse.mkString)
      .map(
      binString =>
        binString.indices.map(
          i =>
            if (binString.charAt(i) == '0') originalString.charAt(i)
            else
              originalString.charAt(i).toUpper
        ).mkString
    )
  }

  def main(args: Array[String]): Unit = {
    println(permute("abcd"))
  }
}
