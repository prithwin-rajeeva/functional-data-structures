package org.scala.algo

object AddTwoNumbersInList {

  def addTwoNumbers(l1: List[Int], l2: List[Int]): List[Int] = {
    val biggerList = if(l1.length > l2.length) l1 else l2
    val smallerList = if(l1.length > l2.length) l2 else l1

    def pipe(res: List[Int], tail: List[Int], sum: Int): (Int, List[Int], List[Int]) = {
      if (sum > 9) {
        (1, res :+ sum % 10, tail)
      } else {
        (0, res :+ sum, tail)
      }
    }

    val result = biggerList.foldLeft((0, List.empty[Int],smallerList)) (
      (acc,item) => acc match {
        case (rem,res,other) => other match {
          case Nil => {
            val sum = item +  rem
            pipe(res, Nil, sum)
          }
          case head :: tail =>
            val sum = item + head + rem
            pipe(res, tail, sum)
        }
      }
    )
    if(result._1 == 0) {result._2} else {result._2 :+ 1}
  }

}
