package org.scala.algo

import scala.annotation.tailrec

object LongestSubstringWithoutRepeats {

@tailrec
  def _lols(
            in: String,
            st: Int,
            en: Int,
            m: Map[Char, Int],
            max: Int
           ): Int = {

    if(en ==  in.length) max
    else {
      if(m.contains(in.charAt(en))) {
        val newSt = math.max(st, m(in.charAt(en)) + 1)
        _lols(
          in,
          newSt,
          en + 1,
          m + (in.charAt(en) -> en),
          math.max(max,en - newSt +1)
        )
      } else {
        _lols(
          in,
          st,
          en + 1,
          m + (in.charAt(en) -> en),
          math.max(max,en - st +1)
        )
      }
    }
  }


  def lengthOfLongestSubstring(s: String): Int = {
    _lols(s,0,0,Map.empty[Char,Int],Int.MinValue)
  }

  def main(args: Array[String]): Unit = {
    println(lengthOfLongestSubstring("au"))
  }




  def lolswr(in:String):Int= {
    def _lolswr(m: Map[Char, Int], st: Int, en: Int, max: Int): Int = {
      if(en == in.length) max
      else {
        if(!m.contains(in.charAt(en))) {
          _lolswr(
            m = m + (in.charAt(en) -> en),
            st,
            en + 1,
            math.max(max,en - st + 1)
          )
        } else {
          val newStart = math.max(st,m(in.charAt(en))+1)
          _lolswr(
            m = m + (in.charAt(en) -> en),
            newStart,
            en + 1,
            math.max(max,en - newStart + 1)
          )
        }
      }

    }
    _lolswr(Map.empty[Char,Int],0,0,Int.MinValue)
  }
}
