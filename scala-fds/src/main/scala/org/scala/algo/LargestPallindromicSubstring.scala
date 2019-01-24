package org.scala.algo

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object LargestPallindromicSubstring {
  def longestPalindromeLength(s: String): Int = {
    @tailrec
    def maxAt(st: Int,en:Int,max:Int):Int = {
      if(st < 0 || en > s.length - 1 || s.charAt(st) != s.charAt(en)) max
      else maxAt(st-1,en+1,max+2)
    }

    def _lp(st:Int,max:Int = Int.MinValue):Int = {
      if(st == s.length) max
      else List(max,maxAt(st-1,st+1,1),maxAt(st,st+1,0),_lp(st+1)).max
    }

    _lp(0)
  }

  def longestPalindrome(s: String): String = {

    @tailrec
    def maxAt(startIndex: Int, endIndex: Int): String = {
      if (startIndex < 0 || endIndex > s.length - 1 || s.charAt(startIndex) != s.charAt(endIndex))
        s.substring(startIndex + 1, endIndex)
      else
        maxAt(startIndex - 1, endIndex + 1)
    }

    @tailrec
    def _lp(st: Int, largestYet: String): String = {
      if (st == s.length) largestYet
      else {
        val largestOdd = maxAt(st - 1, st + 1)
        val largestEven = maxAt(st, st + 1)

        if (largestOdd.length > largestEven.length && largestOdd.length > largestYet.length)
          _lp(st + 1, largestOdd)
        else if (largestEven.length > largestOdd.length && largestEven.length > largestYet.length)
          _lp(st + 1, largestEven)
        else
          _lp(st + 1, largestYet)
      }
    }

    _lp(0, "")
  }


  def getAdjascentNodes(in:String,reg:Set[String]):List[String] = {
    in.indices.flatMap(
      i => {
        for (c <- 'a' to 'z' if c != in.charAt(i)) yield in.substring(0, i) + c + in.substring(i + 1, in.length)
      }
    ).filter(x => reg.contains(x)).toList
  }

  def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int = {
    val wl = wordList.toSet
    def _ll(queue: Queue[String], visited: Set[String], acc: Int): Int = {
      if(queue.isEmpty) 0
      else {
        val next = queue.dequeue
        val top = next._1
        val nextQ = next._2.enqueue(getAdjascentNodes(top,wl))
        if(visited.contains(top)) 0 else {
          val nextV = visited + top
          if(top == endWord) acc
          else {
            _ll(nextQ,nextV,acc+1)
          }
        }
      }
    }
    val queue = Queue.empty[String].enqueue(beginWord)
    val visited = Set.empty[String]
    _ll(queue,visited,1)
  }

  /*
  "qa"
  "sq"
  ["si","go","se","cm","so","ph","mt","db","mb","sb","kr","ln","tm","le","av","sm","ar","ci","ca","br","ti","ba","to","ra","fa","yo","ow","sn","ya","cr","po","fe","ho","ma","re","or","rn","au","ur","rh","sr","tc","lt","lo","as","fr","nb","yb","if","pb","ge","th","pm","rb","sh","co","ga","li","ha","hz","no","bi","di","hi","qa","pi","os","uh","wm","an","me","mo","na","la","st","er","sc","ne","mn","mi","am","ex","pt","io","be","fm","ta","tb","ni","mr","pa","he","lr","sq","ye"]
   */






  def main(args: Array[String]): Unit = {
    println(longestPalindrome("babad"))
  }
}
