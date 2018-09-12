import scala.concurrent.Future

val x = List(Future(1), Future(2), Future(3), Future(4), Future(5)).foldLeft(Future(List.empty[Int]))((a, b) => {
  for {
    u <- a
    v <- b
  } yield {
    u :+ v
  }
})


