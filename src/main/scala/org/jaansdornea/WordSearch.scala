package org.jaansdornea

object WordSearch extends App {

  val grid = Array.ofDim[Char](4, 4)
  val range = (0 until 4)
  range.foreach(i => grid(0)(i) = "ABCD"(i))
  range.foreach(i => grid(1)(i) = "ATBQ"(i))
  range.foreach(i => grid(2)(i) = "LIGE"(i))
  range.foreach(i => grid(3)(i) = "BOBO"(i))

  // row, column
  type position = (Int, Int)

  trait Movement {
    def go(): position
    def canMove(): Boolean
  }

  case class Up(p: position) extends Movement {
    override def canMove(): Boolean = p._1 > 0
    override def go(): position = (p._1 -1, p._2)
  }

  case class Down(p: position) extends Movement {
    override def canMove(): Boolean = p._1 < 3 
    override def go(): position =  (p._1 + 1, p._2) 
  }

  case class Right(p: position) extends Movement {
    override def canMove(): Boolean = p._2 < 3
    override def go(): position = (p._1, p._2 + 1)
  }

  case class Left(p: position) extends Movement {
    override def canMove(): Boolean = p._2 > 0
    override def go(): position = (p._1, p._2 - 1)
  }

  def options(p: position): List[Movement] =
    List(Up(p), Down(p), Right(p), Left(p)).filter((movement) => movement.canMove)

  def search(word: String): Boolean = {
    val slength = word.length()
    
    def check (m: Movement, fragment: String): Boolean = {
      if (m.canMove()) {
        val (row, column) = m.go()
        val char = grid(row)(column)
        char == fragment.head
      } else false
    }
    
    def accum(yarn: List[position], current: position, fragment: String): List[position] = 
      if (fragment.isEmpty) yarn :+ current
      else {
        val movementOptions: List[WordSearch.Movement] = options(current)
        val carryOn = movementOptions.filter( (m) => check (m, fragment))
        if (carryOn.isEmpty) yarn
        else {
          val yarns = carryOn.map( (m) => accum( yarn :+ current, m.go, fragment.tail))
          yarns.maxBy ((l) => l.length)
        }
      }
      
    (0 until 4).foreach( (row) => 
      (0 until 4).foreach( (column) =>
        {
          val startingPosition = (row, column)
          if (grid(row)(column) == word.head) {
            val yarn = accum(List[position](), startingPosition, word.tail)
            println(s" startingPosition: $startingPosition, yarn: $yarn")
            if (yarn.length == word.length) {
              return true
            }
          }
        }
      )
    )
    false
  }
 
  
  println(search("ATIGEO"))

}