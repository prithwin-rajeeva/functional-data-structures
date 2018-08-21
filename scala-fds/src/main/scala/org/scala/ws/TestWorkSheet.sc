import java.util.concurrent.TimeUnit

import scala.collection.immutable.HashMap
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

sealed trait Json
case class JsonObject(obj:Map[String,Json]) extends Json
case class JsonString(s:String) extends Json
case class JsonNumber(n:Int) extends Json
case object JsonNull extends Json

trait JsonWriter[T] {
  def write(value:T):Json
}

case class Person(name:String,age:Int,sex:Boolean)

object writers {
  implicit val personWriter = new JsonWriter[Person] {
    def write(value:Person):Json = {
      JsonObject(Map(
        "name" -> JsonString(value.name),
        "age" -> JsonNumber(value.age),
        "sex" -> (if(value.sex) JsonString("Male") else JsonString("Female"))
      ))
    }
  }

  implicit val stringWriter = new JsonWriter[String] {
    def write(str:String):Json = {
      JsonString(str)
    }
  }
}

object JsonSyntax {
  implicit class JsonWriteOp[A](value:A) {
    def toJson(implicit writer:JsonWriter[A]):Json = {
      writer.write(value)
    }
  }
}

object Json {
  def toJson[A](value:A)(implicit writer:JsonWriter[A]):Json = {
    writer.write(value)
  }
}

object myScope extends App{
  import writers._
  import JsonSyntax._
  def getEither():Either[String,Boolean] = Right(true)
  println(Person("the man",32,true).toJson)
}


val promise = Promise[Int]
Future{
  Thread.sleep(60000)
  promise.success(1)
}

val somethingelse = promise.future
println(
  Await.result(somethingelse.map(_ * 2) , Duration(20,TimeUnit.SECONDS)))

val hashMap = HashMap("this" -> "that")
