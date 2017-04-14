package org.jaansdornea

object LCS3 extends App {

  case class XYZ(val x: Int, val y: Int, val z: Int) {
    def decrimentX(): XYZ = XYZ(x -1, y, z)
    def decrimentY(): XYZ = XYZ(x, y-1, z)
    def decrimentZ(): XYZ = XYZ(x, y, z-1)
    def decrimentAll(): (XYZ, XYZ, XYZ) = (decrimentX, decrimentY, decrimentZ)
  }

  /**
   * we want to calculate the longest common subsequence
   * from three sequences
   * steps:
   * 		1.) if X[i] == Y[j] == Z[k] => 1 + previous length
   * 		2.) if {X, Y, Z}.length == 0 => previous length
   * 		3.) if (X[i] == Y[j] == Z[k]) == false => max(value of i - 1 across vectors)
   */
  def calculateLCS[A](X: Seq[A], Y: Seq[A], Z: Seq[A]): Int = {
    val cache = scala.collection.mutable.Map[XYZ, Int]()
    
    def storeAndReturn(key: XYZ, value: Int): Int = {
      cache.put(key, value)
      value
    }

    def dpCalcLCS(X: Seq[A], Y: Seq[A], Z: Seq[A], xyz: XYZ ): Int = {
      // is requested value previously calculated?
      cache.get(xyz) match {
        case Some(value) => value 
        case None => {
          xyz match {
            case XYZ(xi, yi, zi) => {
          
              // we are no longer within one of our strings, thus cannot have a match
              if (xi < 0 || yi < 0 || zi < 0) 0
          
              // we have a match, we add 1 to the net calculation
              else if (X(xi) == Y(yi) && Y(yi) == Z(zi))
                storeAndReturn(xyz, 1 + dpCalcLCS(X, Y, Z, xyz.decrimentX()))
          
              // no match so we take max in each vector direction
              else {
                val (xdir, ydir, zdir) =xyz.decrimentAll()
                Math.max(storeAndReturn(zdir, dpCalcLCS(X, Y, Z, zdir)),
                  Math.max(storeAndReturn(xdir, dpCalcLCS(X, Y, Z, xdir)), 
                  storeAndReturn(ydir, dpCalcLCS(X, Y, Z, ydir))))
              }
            }
          }
        }
      }
    }

    val initialIndices = XYZ(X.length - 1, Y.length - 1, Z.length - 1)
    dpCalcLCS(X, Y, Z, initialIndices)

  }

  println(calculateLCS("geeks", "ageekfor", "anothergeek"))
  println(calculateLCS[Int](List(1,2,3,5,6), List(3,5,6), List(7,1,3,5)))
}