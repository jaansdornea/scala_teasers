package org.jaansdornea.graph
/**
 * spanning tree of a graph is a subgraph of a graph such that 
 * all nodes in the graph are connected
 * 
 * minimum spanning tree of a graph is a spanning tree who's sum
 * of the edges' weights are the minimum of all spanning trees
 */
object PrimmsMinimumSpanningTree extends App {
  trait Graph
  case class Edge[T](v1: T, v2: T, weight: Int)
  case class SingleVertex[T](v: T) extends Graph
  case object Empty extends Graph
  case class Connected[T](edges: Edge[T]*)
  
  /*
  class MapWithBinaryHeap[T] {
    def extractMin
  }
  */
  
  def minSpanTree(g: Graph): Int = {
    0
  }
  
}