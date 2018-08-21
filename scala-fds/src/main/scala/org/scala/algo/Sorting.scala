package org.scala.algo

object Sorting {
  def selectionSort[T](in: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {
    if (in.length <= 1) in
    else {
      val minPoint = in.zipWithIndex.foldLeft((in.head, 0))(
        (agg, item) =>
          if (ordering.lt(agg._1, item._1))
            agg
          else item
      )
      selectionSort(in.slice(0, minPoint._2) ++ in.slice(minPoint._2 + 1, in.length)).+:(minPoint._1)
    }
  }
}
