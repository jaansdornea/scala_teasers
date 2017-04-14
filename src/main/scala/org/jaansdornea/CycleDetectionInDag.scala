package org.jaansdornea

object CycleDetectionInDag extends App {
 
  case class DagNode(val i: Int, var next: List[DagNode])
  
  def isCycle(start: DagNode): Boolean = {
    // follow all links until end or in visited hashset
    def follow(node: DagNode, visited: Set[Int]): Boolean = {
     if (visited.contains(node.i)) true
     else if (node.next.isEmpty) false
     else {
       node.next.map ( (next) => follow(next, visited + node.i) ).filter( (cycle) => cycle).length > 0
     }
    }
    follow(start, Set[Int]())
  }
  
  
  val n = DagNode(4, List(DagNode(5, List[DagNode]())))
  val root = DagNode(2, List(n, DagNode(3, List(n))))
  assert(!isCycle(root))
  n.next = root :: n.next
  assert(isCycle(root))
  
  //
  val _3 = DagNode(3, List())
  _3.next = List(_3)
  val _2 = DagNode(2, List(_3))
  val _1 = DagNode(1, List(_2))
  val _0 = DagNode(0, List(_2, _1))
  _2.next = List(_3, _0)
  assert(isCycle(_0))
  
}