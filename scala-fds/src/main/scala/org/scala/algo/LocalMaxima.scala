package org.scala.algo

object LocalMaxima {

  val minGap = 0.00000004

  def findMaxValue(f: Double => Double, st: Double, en: Double): Double = {
    def _lm(_st: Double, _en: Double): Double = {
      if (math.abs(_st - _en) < minGap) {
        _st
      } else {
        val mid = (_st + _en) / 2
        val ml: Double = f(mid - minGap/2)
        val mr: Double = f(mid + minGap/2)

        if (ml < mr) {
          _lm(mid + minGap, _en)
        } else if (ml > mr) {
          _lm(_st, mid - minGap)
        } else {
          _lm(mid - minGap, mid + minGap)
        }
      }
    }

    _lm(st, en)
  }
}
