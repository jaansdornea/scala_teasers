package org.jaansdornea

object BoundaryProblem extends App{
 
  def getBoundary(root: Node): List[Int] = {
   root.value :: leftB(root) ::: bottomB(root) ::: rightB(root)  
  }
  
  def leftB(root: Node): List[Int] = {
    def accum(vals: List[Int], next: Node): List[Int] = 
      next.left match {
        case None => 
          next.right match {
            case None => vals // this is a leaf
            case Some(node) => accum(vals :+ next.value, node)
          }
     
        case Some(node) => accum(vals :+ next.value, node) 
      }
    root.left match {
      case None => List[Int]()
      case Some(x) => accum (List[Int](), x)
    }
  }
  
  def bottomB(root: Node): List[Int] = {
    def accum(vals: List[Int], next: Node): List[Int] = 
      if (next.left == None && next.right == None) vals :+ next.value
      else if (next.left == None) accum(vals, next.right.get)
      else if (next.right == None) accum(vals, next.left.get)
      else accum(vals, next.left.get) ::: accum(List[Int](), next.right.get)
        
    accum(List[Int](), root)
  }
  
  def rightB(root: Node): List[Int] = {
    def accum(vals: List[Int], next: Node): List[Int] = 
      next.right match {
        case None => 
          next.left match {
            case None => vals.reverse
            case Some(node) => accum(vals :+ next.value, node)
          }
        case Some(node) => accum(vals :+ next.value, node)  
      }
    root.right match {
      case None => List[Int]()
      case Some(x) => accum(List[Int](), x)
    }  
  }
  
  
  //case class Node (val value: Int, val left: Option[Node] = None, val right: Option[Node] = None)
  val _10 = Node(10, None, None)
  val _14 = Node(14, None, None)
  val _12 = Node(12, Some(_10), Some(_14))
  val _4  = Node(4, None, None)
  val _8  = Node(8, Some(_4), Some(_12))
  val _25 = Node(25, None, None)
  val _22 = Node(22, None, Some(_25))
  val _20 = Node(20, Some(_8), Some(_22))
  
  val boundary = getBoundary(_20)
  println(boundary)
  
}