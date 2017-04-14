package org.jaansdornea

//https://en.wikipedia.org/wiki/Tree_traversal

object TreeTraversals extends App {

  case class Node[V](val value: V, var left: Option[Node[V]] = None, var right: Option[Node[V]] = None)
  def initTestTree(): Node[Char] = {
    def n(c: Char, left: Option[Node[Char]] = None, right: Option[Node[Char]] = None): Node[Char] = {
      Node(c, left, right)
    }
    val C = n('C')
    val E = n('E')
    val D = n('D', Some(C), Some(E))
    val A = n('A')
    val B = n('B', Some(A), Some(D))
    val H = n('H')
    val I = n('I', Some(H))
    val G = n('G', None, Some(I))
    val F = n('F', Some(B), Some(G))
    F
  }

  // traversal 1> preorder
  /*
   * F ------------>  G -----> I
   * B ---->D--> E             H
   * A      C
   * 
   * FBADCEGIH
   */
  
  def preorder(root: Node[Char]): List[Char] = {
    def traverse(n: Option[Node[Char]]): List[Char] = {
      n match {
        case None => List[Char]()
        case Some(node) => node.value :: traverse(node.left) ::: traverse(node.right)
      }
    }
    traverse(Some(root))
  }
  
  println(preorder(initTestTree()))

  // traversal 2> inorder
  /*
   * F ------------>  G -----> I
   * B ---->D--> E             H
   * A      C
   * 
   * ABCDEFGHI
   */
  
  def inorder(root: Node[Char]): List[Char] = {
    def traverse(n: Option[Node[Char]]): List[Char] = {
      n match {
        case None => List[Char]()
        case Some(node) => {
          node.left match {
            case None => node.value :: traverse(node.right)
            case Some(leftNode) => (traverse(node.left) :+ node.value) ::: traverse(node.right)
          }
        }
      }
    }
    traverse(Some(root))
  }
  
  println(inorder(initTestTree()))

  // traversal 3> postorder
  /*
   * F ------------>  G -----> I
   * B ---->D--> E             H
   * A      C
   * 
   * ACEDBHIGF
   */
  
  def postorder(root: Node[Char]): List[Char] = {
    def traverse(n: Option[Node[Char]]): List[Char] = {
      n match {
        case None => List[Char]()
        case Some(node) => (traverse(node.left) ::: traverse(node.right) ) :+ node.value
      }
    }
    traverse(Some(root))
  }

  println(postorder(initTestTree()))
  
}