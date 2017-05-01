package org.jaansdornea.utility
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BSTSpec extends FunSuite with ShouldMatchers {
  
  lazy val testBST = BST[Int](1,2,3,4,5,6).get
  val singleTree = BST[Int](1).get
  
  test("traversals") {
    (testBST.preOrder()) should be (List[Int](4,2,1,3,6,5))
    (testBST.postOrder()) should be (List[Int](1,3,2,5,6,4))
    (testBST.inOrder()) should be (List[Int](1,2,3,4,5,6))
  }
  
  test("size and height") {
    (testBST.size()) should be (6)
    (testBST.height()) should be (3)
    (singleTree.size()) should equal (1)
    (singleTree.height()) should equal (1)
  }
  
  test("insert value") {
    val temp = BST[Int](1,3,4,5,6).get
    temp.insert(2)
    (temp.inOrder()) should be (List[Int](1,2,3,4,5,6))
  }
  
  test("contains") {
    val temp = BST[Int](1,3,4,5,6).get
    (temp.contains(2)) should be (false)
    List(1,3,4,5,6).foreach { (i) => (temp.contains(i)) should be (true) }
  }
  
  test("remove") {
    val temp = BST[Int](1,2,3,4,5,6).get
    temp.remove(2)
    (temp.contains(2)) should be (false)
    (temp.inOrder()) should be (List[Int](1,3,4,5,6))
  }
  
}