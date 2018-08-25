import scala.concurrent.Future

val x = Future.traverse(List(1,2,3))(x =>Future{x})

