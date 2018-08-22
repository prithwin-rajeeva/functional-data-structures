package org.scala.mt

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.{Condition, Lock, ReentrantLock}

import com.sun.tools.javadoc.Start

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}

object SyncroProbs {

   def twoThreadTurnBased():Unit = {
    val sharedLock = new ReentrantLock
    val condition = sharedLock.newCondition
    val even = new AtomicBoolean(false)
    val latch = new CountDownLatch(2)

   Future {
     {
       var i = 1
       while(i < 20) {
         sharedLock.lock()
         while(even.get) condition.await()
         println(i)
         even.set(true)
         condition.signalAll()
         sharedLock.unlock()
         i = i + 2
       }
       latch.countDown()
     }
   }
    Future {
      {
        var i = 2
        while(i < 20) {
          sharedLock.lock()
          while(!even.get) condition.await()
          println(i)
          even.set(false)
          condition.signalAll()
          sharedLock.unlock()
          i = i + 2
        }
        latch.countDown()
      }
    }

    latch.await()
  }

  def main(args: Array[String]): Unit = {
    twoThreadTurnBased
  }
}
