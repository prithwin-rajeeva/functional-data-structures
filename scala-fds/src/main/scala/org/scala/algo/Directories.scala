package org.scala.algo

object Directories {

  trait FSObj {
    def name: String
    def children: Set[FSObj]
    def add(fSObj: FSObj):FSObj
    def prettyPrint:String
  }

  case class File(name: String) extends FSObj {
    override def children: Set[FSObj] = Set.empty[FSObj]
    override def add(fSObj: FSObj): FSObj = throw new IllegalAccessException("cant add anything to a file")
    def prettyPrint: String = name
  }

  case class Direcotry(
                        name: String,
                        children: Set[FSObj] = Set.empty[FSObj]
                      ) extends FSObj {
    override def add(fSObj: FSObj): FSObj = this.copy(children = children + fSObj)

    override def prettyPrint: String = ""
  }



  def main(args: Array[String]): Unit = {
    val root = Direcotry(
      "root",
      Set(
        Direcotry(
          "A",
          Set(
            File("B"),
            File("C"),
            Direcotry(
              "D",
              Set(File("E"),Direcotry("F"))
            )
          )
        )
      )
    )

    println(root.prettyPrint(0))
  }
}
