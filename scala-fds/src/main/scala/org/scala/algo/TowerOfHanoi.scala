package org.scala.algo

object TowerOfHanoi {
  def printMoves(n: Int): Unit = {
    def _printMoves(x: Int, source: String, temp: String, dest: String): Unit = {
      if (x == 1) println(s"$source -> $dest") else {
        _printMoves(x - 1, source, dest, temp)
        println(s"$source -> $dest")
        _printMoves(x - 1, temp, source, dest)
      }

    }
    _printMoves(n,"A","B","C")
  }
}