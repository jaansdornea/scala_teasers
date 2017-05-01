package org.jaansdornea.utility

object GradientDescent extends App {
  val stepSize = 0.05
  val numberOfIterations = 5000
  val features = Array[Double](1.0, 2.0, 3.0)
  val target = Array[Double](1.0, 2.0, 3.0)
  
  val gd = new GradientDescent(stepSize, numberOfIterations, features, target)
  println(gd.converge())
}

/**
 * used for minimization of cost function with features fi 0 -> N where N = 1
 */
class GradientDescent(val stepSize: Double, val iterationThreshold: Int,
        /* single feature */featureMatrix: Array[Double], targetVector: Array[Double], 
        val convergenceDelta: Double = 0.00001) {

  require(!featureMatrix.isEmpty, "need features for GD")
  require(featureMatrix.length == targetVector.length, "need target values for all features")

  val m = featureMatrix.length
  val invm = 1.0 / targetVector.length
  def theta0PartialDelta(theta0: Double, theta1: Double, target: Double, feature: Double): Double =
    2 * (theta0 + theta1 * feature - target)
  def theta1PartialDelta(theta0: Double, theta1: Double, target: Double, feature: Double): Double =
    2 * (theta0 + theta1 * feature - target) * feature

  // model params vector
  val modelParams = Array[Double](0.0, 0.0)

  def updateFeatureGuess(theta0: Double, theta1: Double): (Double, Double) = {
    val incremented0 = theta0 - (stepSize / m) *
      (0 until m).foldLeft(0.0)((sum, index) =>
        sum + theta0PartialDelta(theta0, theta1, targetVector(index), featureMatrix(index)))
    val incremented1 = theta1 - (stepSize / m) *
      (0 until m).foldLeft(0.0)((sum, index) =>
        sum + theta1PartialDelta(theta0, theta1, targetVector(index), featureMatrix(index)))
    (incremented0, incremented1)
  }

  def converge(): (Double, Double) = {
    var currentIteration = 0
    var delta = Double.MaxValue
    while (delta > convergenceDelta && currentIteration < iterationThreshold) {
      val updated = updateFeatureGuess(modelParams(0), modelParams(1))
      
      delta = Math.pow(Math.pow((updated._1 - modelParams(0)),2) + 
          Math.pow((updated._2 - modelParams(1)),2), 0.5)
          
      println(s"delta: => $delta")
      modelParams(0) = updated._1; modelParams(1) = updated._2
      currentIteration += 1
    }
    (modelParams(0), modelParams(1))
  }

}