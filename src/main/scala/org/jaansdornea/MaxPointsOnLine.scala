package org.jaansdornea

import scala.annotation.tailrec

/**
 * Given n points on a 2D plane, find the maximum number of points that lie on the same straight line.
 */

object MaxPointsOnLine extends App {
  // (x, y)
  type Point = (Double, Double)

  def gcd(x: Int, y: Int): Int = {
    if (x == 0 || y == 0) return 1

    val (absx, absy) = (Math.abs(x), Math.abs(y))
    val (min, max) = if (absx <= absy) (absx, absy)
    else (absy, absx)
    val range: IndexedSeq[Int] = (1 to Math.max((min / 2), 2)).filter(min % _ == 0) :+ min
    val gcd = range.foldLeft(1)(
      (maxdivisor, potentialdivisor) => if (max % potentialdivisor == 0) potentialdivisor else maxdivisor)
    gcd
  }

  // (numerator, denominator)
  case class Fraction(num: Int, dem: Int = 1) {
    val (numerator, denominator): (Int, Int) = {
      if (dem == 0) throw new IllegalStateException("cannot divide by zero")
      else if (dem < 0) (-1 * num, -1 * dem)
      else (num, dem)
    } 
    
    override def toString = s"${numerator}/${denominator}"
    private def reduce: Fraction = {
      val cd = gcd(this.numerator, this.denominator)
      Fraction(this.numerator / cd, this.denominator / cd)
    }

    def +(x: Fraction) =
      Fraction(numerator * x.denominator + x.numerator * denominator, (denominator * x.denominator)).reduce

    def -(x: Fraction) =
      Fraction(numerator * x.denominator - x.numerator * denominator, (denominator * x.denominator)).reduce

    def *(x: Fraction) =
      Fraction(numerator * x.numerator, denominator * x.denominator).reduce

    def /(x: Fraction) = this * Fraction(x.denominator, x.numerator)

  }

  /* fraction tests
   * ---------------
  assert(Fraction(1,2) + Fraction(2) == Fraction(5,2))
  assert(Fraction(2,3) - Fraction(2,4) == Fraction(1,6))
  assert(Fraction(1) - Fraction(1) == Fraction(0))
  assert(Fraction(1,2) * Fraction(2) == Fraction(1))
  assert(Fraction(2,8) * Fraction(1,2) == Fraction(1,8))
  assert(Fraction(1,2) / Fraction(2) == Fraction(1,4))
  assert(Fraction(2,8) / Fraction(2) == Fraction(1,8))
  * ---------------
  * */
  

  // (yIntercept, slope)
  trait Line
  case class StandardLine(val yIntercept: Fraction, val slope: Fraction) extends Line
  case class HorizontalLine(val yIntercept: Fraction) extends Line
  case class VerticalLine(val xIntercept: Fraction) extends Line

  def convert(x: Double): Fraction = {
    if (x - x.toInt == 0) /* no decimal part */ Fraction(x.toInt)
    else {
      val leftSide = x.toInt
      val rightSideString = (x - leftSide).toString.split('.')(1)
      val rightLength = rightSideString.length()
      Fraction(leftSide) + Fraction(rightSideString.toInt, Math.pow(10, rightLength).toInt)
    }
  }

  /*
  val oneandahalf = convert(1.5)
  assert(oneandahalf == Fraction(3, 2))
  val onehalf = convert(0.5)
  assert(onehalf == Fraction(1, 2))
	*/
  
  //1.) get powerset of all point tuples with line def
  def lineFromPoints(a: Point, b: Point): Line = {
    val slopeDenominator = (convert(a._1) - convert(b._1))
    val slopeNumerator = (convert(a._2) - convert(b._2))
    if (slopeDenominator == Fraction(0)) // have vertical line
      VerticalLine(convert(b._1))
    else if (slopeNumerator == Fraction(0)) // have horizontal line
      HorizontalLine(convert(b._2))
    else {
      val slope = {
        val num = (convert(a._2) - convert(b._2))
        val den = (convert(a._1) - convert(b._1))
        num/den
      }
      val yIntercept = convert(a._2) - (slope * convert(a._1))
      StandardLine(yIntercept, slope)
    }
  }

  def lines(points: List[Point]): List[Line] = {

    @tailrec
    def accumulate(accum: List[Line], restOfPoints: List[Point]): List[Line] = {
      restOfPoints match {
        case Nil         => accum
        case head :: Nil => accum
        case head :: tail => {
          val newAccum: List[Line] = accum ::: tail.map((p) => lineFromPoints(head, p))
          accumulate(newAccum, restOfPoints.tail)
        }
      }
    }

    accumulate(List[Line](), points)
  }
  
  def continuous(l1: Line, l2: Line): Boolean = {
    l1 match {
      case StandardLine(intercept, slope) => {
        l2 match {
          case StandardLine(i, s) if i == intercept && s == slope => {
            println(s"$l2")
            true
          }
          case _ => {
            false
          }
        }
      }
      case HorizontalLine(yintercept) => { 
        l2 match {
          case HorizontalLine(yi) if yi == yintercept => {
            true
          }
          case _ => {
            false
          }
        }
      }
      case VerticalLine(xintercept) => { 
        l2 match {
          case VerticalLine(xi) if xi == xintercept => {
            true
          }
          case _ => {
            false
          }
        }
      }
    }
  }

  //2.) fold over all lines folding over all points updating click per line
  def findMax(points: List[Point]): Int = {
    val possibleLines = lines(points).zipWithIndex
    val maxEqualLines = (0 until (possibleLines.length)).map {
      case (index) =>
        { possibleLines.filter { 
            case (l, i) => 
              i != index && continuous(possibleLines(index)._1, possibleLines(i)._1)
             }
        }.length
    }.max
    val possibilities = (1 to (2 * (maxEqualLines)))
    val maxNumPointsOnLine = possibilities(possibilities.takeWhile((i) => ((i * i) - i <= (maxEqualLines) * 2)).size)
    maxNumPointsOnLine
  }

  /*val points = List[Point]((-2.0, -3.0), (-1.0, -1.0), (1.0, -1.75), (4.0, -4.0), (2.0, -2.0), (3.0, -3.0), (5.0, -2.0),
    (5.0, 1.0), (1.0, 1.0), (1.0, 1.5), (2.0, 3.0), (3.0, 6.0), (5.0, 10.0), (6.0, 12.0), (3.5, 7.0), (2.0, 4.0) )
    */
  
  val points = List[Point]((5.0, 1.0), (5.0, 2.0), (5.0, 3.0))
  val maxPoints = findMax(points)
  assert(maxPoints == 3)

}