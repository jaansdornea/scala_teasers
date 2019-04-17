package org.jaansdornea

package object tree {

  case class Point(x: Double, y: Double)
  case class Line(start: Point, end: Point)

  case class Measurement(count: Int, derivative: Int)
  case class Data(friendAg: Measurement, tradeAg: Measurement)

  // position in list is how many lines have gone through that location
  type Tree = Map[Int, (Map[Point, Set[Line]], Map[Point, Set[Line]])]

  // get the random length...
  val branchLength: () => Double = () => Math.random() * 2.0

  // get the random slope
  val branchSlope: () => Double = () => Math.random() * {
    if (Math.random() > 0.5) 1
    else -1
  }

  def distance(l: Line): Double = {
    math.pow(math.pow(math.abs(l.start.x - l.end.x), 2) +
      math.pow(math.abs(l.start.y - l.end.y), 2), 0.5)
  }

  def draw(start: Point, length: Double, slope: Double): Line = {
    /*
     * c = 1/sqrt(1 + m^2),
     * s = m/sqrt(1 + m^2)
     * end = Poin(start.x, start.y +|- d(c, s))
     */

    val c = 1.0 / math.pow(1 + math.pow(slope, 2.0), 0.5)
    val s = slope / math.pow(1 + math.pow(slope, 2.0), 0.5)

    // assume lines always moving up
    if (slope >= 0)
      Line(start, Point(start.x + length * c, start.y + length * s))
    else Line(start, Point(start.x - length * c, start.y + length * s))

  }

}