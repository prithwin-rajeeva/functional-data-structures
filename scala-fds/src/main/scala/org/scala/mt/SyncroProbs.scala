package org.scala.mt

import java.util.concurrent.{CountDownLatch, Executors, LinkedBlockingDeque, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.{Condition, Lock, ReentrantLock}

import com.sun.tools.javadoc.Start

import scala.collection.{immutable, mutable}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

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

  def pingara():Unit = {
    class NB {
      val lock = new ReentrantLock
      val pingOrPong = new AtomicBoolean(true)
      val change = lock.newCondition

      def writer(action:String):Unit = {
          lock.lock()
          if(action == "ping") {
            while(pingOrPong.get() == false) change.await()
          } else {
            while(pingOrPong.get() == true) change.await()
          }
          println(action)
          pingOrPong.set(if(action == "ping") false else true)
          change.signalAll()
          lock.unlock()
      }
    }
    val nb = new NB

    val t1 = new Thread(new Runnable {
      override def run(): Unit = {
        (0 to 100).foreach(_ => {
          nb.writer("ping")
        })
      }
    })


    val t2 = new Thread(new Runnable {
      override def run(): Unit = {
        (0 to 100).foreach(_ => {
          nb.writer("pong")
        })
      }
    })
    t1.start
    t2.start
    t1.join
    t2.join

  }

  def oneAfterOther():Unit = {
    println("this is going to make a lot of difference what we do here")
    val countDownLatch = new CountDownLatch(2)
    val t1 = new Thread(new Runnable {
      override def run(): Unit = (1 to 50 ).foreach(println)
    })
    val t2= new Thread(new Runnable {
      override def run(): Unit = (1 to 50 ).foreach(println)
    })
    t1.start()
    t2.start()
    t1.join()
    t2.join()

  }

  def prodcon():Unit = {
    val mutableQueue: mutable.Queue[Int] = scala.collection.mutable.Queue[Int](5)
    val mutex = new ReentrantLock
    val full = mutex.newCondition()
    val empty = mutex.newCondition()

    val c = new CountDownLatch(2)
    Future {
      (1 to 100).foreach { i =>
        Thread.sleep(1000)
        println(s"enqueing $i")
        try {
          if (mutableQueue.length > 100) throw new IllegalStateException()
          mutex.lock()
          while(mutableQueue.length > 100) full.await()
          mutableQueue.enqueue(i)
          empty.signal()
          mutex.unlock()
      }catch {
          case e => println(e)
        }

      }
      c.countDown()
    }

    Future {
      (1 to 100).foreach { i =>
        Thread.sleep(1000)
        try {
          mutex.lock()
          while(mutableQueue.length < 1) empty.await()
          println(s"dequeing ${mutableQueue.dequeue()}")
          full.signal()
          mutex.unlock()
        }catch {
          case e => println(e)
        }
      }
      c.countDown()
    }
    c.await()

  }

  def everyFutureOperationEver(): Unit = {
    val myfut = Future {
      1
    }
    val x = myfut.failed
    for (y <- x) yield (println(y))
    println()
  }

  def main(args: Array[String]): Unit = {
//    twoThreadTurnBased
    //threadPoolSimulator
//    promiseDemo
   // pingara
    //oneAfterOther
//    prodcon
//    everyFutureOperationEver

    def comb(in: String): List[String] = {
      println(in)
      in match {
        case unit if unit.length <= 1 => List(unit)
        case nz =>
          nz.indices.toList.flatMap(
            i => comb(nz.substring(0, i) + nz.substring(i, nz.length))
          )

      }
    }


    val g = comb("abcd")
  }



}
