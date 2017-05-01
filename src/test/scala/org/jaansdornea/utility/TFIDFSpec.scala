package org.jaansdornea.utility

import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TFIDFSpec extends FunSuite with ShouldMatchers {
  
  test("1 word delta") {
    val doc1 = Array[String]("the", "dog", "is", "red")
    val doc2 = Array[String]("the", "dog", "is", "not", "red")
    
    val corpus = Array[Array[String]](doc1, doc2)
    val calculator = TFIDF(corpus)
    calculator.on("red", doc1) should be (0.0)
    (calculator.on("not", doc2)  > 0.0) should be (true)
  }
  
}