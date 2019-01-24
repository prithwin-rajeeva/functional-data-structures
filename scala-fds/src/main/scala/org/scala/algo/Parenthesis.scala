package org.scala.algo

import org.scala.fds.StackAdt
import scala.collection.immutable.Stack

object Parenthesis {
  def isValid(in:Seq[Char]):Boolean = {
     def _process(st:StackAdt[Char],rem:Seq[Char]):Boolean = {
       if(rem.isEmpty) st.isEmpty
       else {
         if (rem.head == '(') _process(st.push(rem.head),rem.tail)
         else {
           if (rem.head == ')' && !st.isEmpty && st.peek == '(') _process(st.pop,rem.tail)
           else false
         }
       }
     }
    _process(StackAdt.empty,in)
  }


  def isValidx(s:Stack[Char],rem:String):Boolean = {
    rem.headOption match {
      case Some(c) if c == '(' => isValidx(s.push(c),rem.tail)
      case Some(c) if c == ')' && s.nonEmpty && s.head == '(' => isValidx(s.pop,rem.tail)
      case _ => false
    }
  }

  def generate(n:Int):List[String] = {
    def _g(start:Int,end:Int,subacc:String,resAcc:List[String]):List[String] = {
      if(start > end) resAcc else  {
        if(start == 0 && end == 0) resAcc :+ subacc
        else {
          val starters = if(start > 0) _g(start - 1 , end , subacc + "(" ,resAcc) else resAcc
          if(end > 0) _g(start,end - 1,subacc+")",starters) else starters
        }
      }

    }
    _g(n,n,"",List.empty[String])
  }


  val startList = Array('(','{','[')
  val endList = Array(')','}',']')

  def isValid(s: String): Boolean = {
    def _isValid(r:List[Char], s:Stack[Char]): Boolean = {
      r.headOption match {
        case Some(c) if startList.contains(c) => _isValid(r.tail, s.push(c))
        case Some(c) if endList.contains(c) => s.headOption match {
          case Some(k) if k == startList(endList.indexOf(c)) => _isValid(r.tail, s.pop)
          case _ => false
        }
        case _ => s.isEmpty
      }
    }

    _isValid(s.toCharArray.toList, Stack.empty[Char])
  }
}
