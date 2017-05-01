package org.jaansdornea.graph

object TopologicalSort extends App {
 
  case class Node[T](val value: T, children: Node[T]*)
  
  def initTestDag(): Set[Node[Char]] = {
    var holder = scala.collection.mutable.Set[Node[Char]]()
    def add(x: Node[Char]): Node[Char] = {holder.add(x); x}
    val g = add (Node[Char]('G'))
    val f = add (Node[Char]('F', g))
    val e = add (Node[Char]('E', f))
    val d = add (Node[Char]('D', f))
    val c = add (Node[Char]('C', e))
    val b = add (Node[Char]('B', c,d))
    val a = add (Node[Char]('A', c))
    Set() ++ holder
  }
  
  def sort(s: Set[Node[Char]]): scala.collection.mutable.Stack[Char] = {
    var visited = scala.collection.mutable.Set[Char]()
    var ordered = scala.collection.mutable.Stack[Char]()
    
    // if not previously visited, 
    //      1.) throw in visited and check if there are children
    //        a. ) if no children put in ordered
    //        b. ) if children in visited put in ordered
    
    def process(n: Node[Char]): Unit = {
      if (!visited.contains(n.value)) {
        n match {
          case Node(_, children @ _*) => {
            children foreach { child => process(child) }
          }
        }
        ordered.push(n.value)
        visited.add(n.value)
      }
    }
    
    s foreach { node => process(node) }
    ordered
  }
  
  println ( sort(initTestDag()) )
  
}