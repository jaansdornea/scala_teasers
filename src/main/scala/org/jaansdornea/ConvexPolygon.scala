package org.jaansdornea

case class Point(val x: Double, val y: Double) {
  def distance(to: Point): Double = {
    // ((x1 -x2)^2 + (y1 - y2)^2)^(1/2)
    Math.pow((Math.pow((x - to.x), 2) + Math.pow((y - to.y), 2)), 0.5)
  }
  
  override def toString(): String = s"($x, $y)"
  
}

case class Polygon(val vertices: Array[Point])

object ConvexPolygon extends App {
  
  def findAveragePoint(vertices: Array[Point]): Point = {
    val (sumx, sumy) = vertices.foldLeft((0.0,0.0)){case (xysums, point) => (xysums._1 + point.x, xysums._2 + point.y) }
    Point(sumx/vertices.length, sumy/vertices.length)
  }
  
  def findFarthestPoints(p: Polygon): (Point, Point) = {
    val middlePoint = findAveragePoint(p.vertices)
    val distances = p.vertices.map( (vertex) => (vertex.distance(middlePoint), vertex)).sortWith((d1, d2) => d1._1 > d2._1)
    (distances.head._2, distances.tail.head._2)
  }
  
  
  val polygon = Polygon(Array[Point](Point(1.0,1.0), Point(1.5, 0.5), Point(2.0, 1.0), Point(1.5, 2.0)))
  println(findFarthestPoints(polygon))
  
}