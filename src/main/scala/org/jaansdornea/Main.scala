package org.jaansdornea

import org.jaansdornea.tree._
import org.jaansdornea.utility._

object Main extends App {
  
  val trunk: Line = Line(Point(1.0, 1.0), Point(1.0, 2.0))
  val testTree: Tree =
    Map[Int, (Map[Point, Set[Line]], Map[Point, Set[Line]])](
      (1 -> (Map[Point, Set[Line]](Point(1.0, 1.0) -> Set(trunk)), Map[Point, Set[Line]](Point(1.0, 2.0) -> Set(trunk)))))
      
  val endTree = (0 to 10).foldLeft(testTree) { case (tree, _) => 
    {
      addBranch(tree)
    }
  }
  prettyPrint(endTree)

}
 
