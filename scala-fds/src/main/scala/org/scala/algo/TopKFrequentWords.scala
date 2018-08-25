package org.scala.algo

import scala.collection.immutable.TreeMap

object TopKFrequentWords {
  def topKFrequent(words: Array[String], k: Int): List[String] = {
    implicit val ordering = implicitly[Ordering[Int]].reverse

    val occByWord = words.foldLeft(Map.empty[String, Int])((inMap, inString) => inMap + (inString ->
      (inMap.getOrElse(inString, 0) + 1)))

    val wordsByOcc = occByWord.foldLeft(TreeMap.empty[Int, List[String]])(
      (inMap, wo) => {
        val (word, occurence) = wo
        inMap + (occurence -> (inMap.getOrElse(occurence, List.empty[String]) :+ word).sorted)
      }
    )
    wordsByOcc.foldLeft(List.empty[String])(_ ++ _._2).take(k)
  }
}
