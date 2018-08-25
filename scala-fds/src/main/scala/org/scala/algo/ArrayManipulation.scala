package org.scala.algo

object ArrayManipulation {
  def reverse[T](arr: Array[T]): Array[T] = {
    def _r(ar: Array[T], st: Int, en: Int): Array[T] = {
      if (st < en) {
        var temp = ar(st)
        ar(st) = ar(en)
        ar(en) = temp
        _r(ar, st + 1, en - 1)
      } else {
        ar
      }
    }
    _r(arr,0,arr.length - 1)
  }

  /**
    * (i)(j) ------- (j)(n - i - 1)
    * |                     |
    *
    * |                     |
    * (n- j - 1)(i)--------(n - i - 1)(n-j-1)
    */
  def rotateMatrixLeft[T](arr:Array[Array[T]]):Array[Array[T]] = {
    var i = 0
    var n = arr.length
    while(i < n /2) {
      var j = i
      while(j < n -i - 1) {
        var temp = arr(i)(j)
        arr(i)(j) = arr(j)(n-i-1)
        arr(j)(n-i-1) = arr(n - i - 1)(n-j-1)
        arr(n - i - 1)(n-j-1) = arr(n- j - 1)(i)
        arr(n- j - 1)(i) = temp
        j+=1
      }
      i+=1
    }
    arr
  }

  def rotateMatrixRight[T](arr:Array[Array[T]]):Array[Array[T]] = {
    var i = 0
    var n = arr.length
    while(i < n /2) {
      var j = i
      while(j < n -i - 1) {
        var temp = arr(i)(j)
        arr(i)(j) = arr(n- j - 1)(i)
        arr(n- j - 1)(i) = arr(n - i - 1)(n-j-1)
        arr(n - i - 1)(n-j-1) = arr(j)(n - i - 1)
        arr(j)(n - i - 1) = temp
        j+=1
      }
      i+=1
    }
    arr
  }
}

