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

  def mergeSort[T](in: Seq[T])(implicit ordering: Ordering[T]): Seq[T] = {
    if (in.length <= 1) in
    else {
      val mid = in.length / 2
      Merger.merge(mergeSort(in.slice(0, mid)), mergeSort(in.slice(mid, in.length)))
    }
  }

  def bubbleSort[T](arr: Array[T])(implicit ordering: Ordering[T]): Array[T] = {

    def _bs(sp: Int, arr: Array[T]): Array[T] = {
      if (sp == arr.length) arr
      else {
        var i = arr.length - 1
        while (i > sp) {
          if (ordering.gt(arr(i - 1), arr(i))) {
            var temp = arr(i - 1)
            arr(i - 1) = arr(i)
            arr(i) = temp
          }
          i -= 1
        }
        _bs(sp + 1, arr)
      }
    }

    _bs(0, arr)
  }
}
