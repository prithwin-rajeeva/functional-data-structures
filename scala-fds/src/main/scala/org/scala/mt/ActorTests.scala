package org.scala.mt

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object ActorTests {
  trait Celluloid

  case object Ping extends Celluloid
  case object Pong extends Celluloid

  class Pinger extends Actor {
    override def receive: Receive = {
      case Pong => println("Ping")
        context.actorSelection("../ponger") ! Ping

    }

  }

  class Ponger extends Actor {
    override def receive: Receive = {
      case Ping => println("Pong")
        context.actorSelection("../pinger") ! Pong
    }

  }
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("TT")
    val pingerref = system.actorOf(Props(new Pinger),"pinger")
    val poingerref = system.actorOf(Props(new Ponger),"ponger")
    pingerref ! Pong

  }
}
