package org.scala.fds

import scala.collection.immutable.Map

case class PTree(sub:scala.collection.immutable.Map[Char,PTree], ends:Boolean, count:Int) {

  def insert(str:String):PTree = {
    if(str.isEmpty) this.copy(ends = true,count = count+1)
    else  {
      val head = str.head
      val tail = str.tail
      if(sub.contains(head)) {
        PTree(
          sub = sub + (head -> sub(head).insert(tail)),
          ends = false,
          count = count
        )
      } else {
        val nextSub = sub + (head -> PTree.empty)
        PTree(
          sub = sub + (head -> nextSub(head).insert(tail)) ,
          ends = false,
          count = count
        )
      }
    }
  }

  def contains(word:String):Boolean = {
     if(word.isEmpty) {
       this.ends
     } else {
       val head = word.head
       val tail = word.tail
       if(!sub.contains(head)) false
       else {
         sub(head).contains(tail)
       }
     }
  }

}

object PTree {
  def empty: PTree = new PTree(Map.empty[Char,PTree],false,0)

  def make(words:String*):PTree = {
    words.foldLeft(empty)((trie,word) => trie.insert(word))
  }

  def main(args: Array[String]): Unit = {
    val item = make("this","that")
    println("hold")
    println(item.contains("this"))
    println(item.contains("that"))
    println(item.contains("tha"))
  }
}
