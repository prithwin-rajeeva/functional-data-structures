package org.scala.events

class CommandService {

}

class QueryService {

}


case class X(a:Int,b:Int)

object EventStore {
  type Time = Double
  type Distance = Double
  type Speed = (Distance,Time) => Double
  type Acceleration = (Speed,Time) => Double

  def terminalVelocity(distance: Distance,acc: Acceleration) = {

  }
}


