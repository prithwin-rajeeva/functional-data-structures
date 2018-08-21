package org.scala.algo

object ProductArray {
  def getProductSeq(in:Seq[Int]):Seq[Int] =
    for(i <- in.indices) yield (in.slice(0, i) ++ in.slice(i + 1, in.length)).product

  def getProductEff(in:Seq[Int]):Seq[Int] = {
    val fromLeft = in.foldLeft((1, Seq.empty[Int]))((ac, i) => (i * ac._1, ac._2 :+ ac._1))._2
    val fromRight = in.reverse.foldLeft((1,Seq.empty[Int]))((ac,i) => (i * ac._1,ac._2 :+ ac._1))._2.reverse
    in.indices.map(i => fromLeft(i) * fromRight(i))
  }
}
