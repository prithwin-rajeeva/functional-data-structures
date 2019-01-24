package org.scala.algo

import scala.collection.mutable

object WordBreak {

    def wordBreak(s: String, wordDict: List[String]): Boolean = {
      val is = wordDict.toSet
      type Input = (String,String)
      val mem = mutable.HashMap.empty[Input,Boolean]

      def memGet(lead:String,trail:String):Boolean = {
        if(mem.contains((lead,trail)))
          mem((lead,trail))
        else {
          mem += ((lead,trail) -> _wb(lead,trail))
          mem((lead,trail))
        }
      }

      def _wb(lead:String,trail:String):Boolean = {
        if(lead.isEmpty && trail.isEmpty) true
        else if(is.contains(lead)) memGet("",trail) || memGet(lead + trail.head,trail.tail)
        else if(!is.contains(lead) && trail.nonEmpty) memGet(lead + trail.head,trail.tail)
        else false

      }
      _wb("",s)
    }

}
