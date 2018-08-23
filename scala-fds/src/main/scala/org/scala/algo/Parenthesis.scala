package org.scala.algo

import org.scala.fds.StackAdt

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
}
