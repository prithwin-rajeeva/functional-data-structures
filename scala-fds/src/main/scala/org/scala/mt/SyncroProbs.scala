package org.scala.mt

import java.util.concurrent.{CountDownLatch, LinkedBlockingDeque, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.{Condition, Lock, ReentrantLock}

import com.sun.tools.javadoc.Start

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

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

  def threadPoolSimulator():Unit = {
    val jobQueue = new LinkedBlockingDeque[Runnable]
    (1 to 2).foreach(i => Future{
      while(true) {
        val runnable = jobQueue.take()
        println(s"runnable received by thread pool = $i ${Thread.currentThread().getName}")
        runnable.run

      }
    })
    //main thread sleeps for the next 5 seconds
    Thread.sleep(5000)
    jobQueue.put(new Runnable {
      override def run(): Unit = {
        println("task1 created")
        Thread.sleep(2000)
      }
    })
    jobQueue.put(new Runnable {
      override def run(): Unit = {
        println("task2 created")
        Thread.sleep(2000)
      }
    })
    jobQueue.put(new Runnable {
      override def run(): Unit = {
        println("task3 created")
        Thread.sleep(2000)
      }
    })
    jobQueue.put(new Runnable {
      override def run(): Unit = {
        println("task4 created")
        Thread.sleep(2000)
      }
    })
    jobQueue.put(new Runnable {
      override def run(): Unit = {
        println("task5 created")
        Thread.sleep(2000)
      }
    })
    new CountDownLatch(3).await()
  }

  def promiseDemo():Unit = {
    val prom = Promise[Int]
    Future {
      Thread.sleep(2000)
      prom.success(34)
    }
    prom.future
  }

  def main(args: Array[String]): Unit = {
//    twoThreadTurnBased
    threadPoolSimulator
//    promiseDemo
  }
}
