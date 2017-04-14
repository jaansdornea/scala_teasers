package org.jaansdornea

import scala.annotation.tailrec

case class NodeString(val entire: String, var left: Option[NodeString], var right: Option[NodeString])

object TreeString extends App{
  
  // assume the input comes in as a full node i.e.(5(3()())(7()()))
  def split(representation: String): (Option[Int], String, String) = {    
    // start with first '(' on stack
    @tailrec
    def accum(stack: List[Char], rest: String, accumulator: String):String = 
      if (stack.isEmpty) accumulator.toString
      else {
        val (_stack, _rest, _accum) = 
          if (rest.head == ')') (stack.tail, rest.tail, accumulator + rest.head)
          else if (rest.head == '(') (rest.head :: stack, rest.tail, accumulator + rest.head)
          else (stack, rest.tail, accumulator + rest.head)
          
        println(s"accum('${_stack}', '${_rest}', '${_accum}')")
        accum(_stack, _rest, _accum)
      }
    
    def nodeValueAndLeftRightAggregated(representation: String): (Option[Int], String) = {
      val inner = representation.tail.dropRight(1)
      if (!inner.isEmpty() && inner.head.isDigit) (Some(inner.takeWhile((d) => d.isDigit).toInt), inner.dropWhile((d) => d.isDigit))
      else (None, representation)
    }
    
    val (valueOption, leftRight) = nodeValueAndLeftRightAggregated(representation)
    valueOption match {
      case None => (None, "", "")
      case Some(x) => {
        val left = accum(List[Char]('('), leftRight.tail, "(")
        val right = accum(List[Char]('('), leftRight.substring(left.length + 1), "(")
        (Some(x), left, right)
      }
    }
    
  }
  
  def buildTree(representation: String): Node = {
    val (root, left, right) = split(representation)
    val n = Node(root.get, None, None) // know that there is a value for the root
    
    def fleshOut(parent: Node, leftString: String, rightString: String): Unit = {
      val (leftChildValOption, leftGrandChildString, rightGrandChildString) = split(leftString)
      val (rightChildValOption, leftGrandChildStringRS, rightGrandChildStringRS) = split(rightString)
      leftChildValOption match {
        case Some(x) => {
          val left = Node(x)
          fleshOut(left, leftGrandChildString, rightGrandChildString)
          parent.left = Some(left)
        }
        case None => None
      }
      rightChildValOption match {
        case Some(x) => {
          val right = Node(x)
          fleshOut(right, leftGrandChildStringRS, rightGrandChildStringRS)
          parent.right = Some(right)
        }
        case None => None
      }
    }
    
    fleshOut(n, left, right)
    n
  }
  
  val root = buildTree("(20(8()())(22(21()())(25(23()())(27(26()())(31()())))))")
  assert (root.left.get.value == 8)
  assert (root.right.get.value == 22)
  assert (root.right.get.left.get.value == 21)
  assert (root.right.get.right.get.value == 25)
  assert (root.right.get.right.get.right.get.value == 27)
  assert (root.right.get.right.get.right.get.left.get.value == 26)
  assert (root.right.get.right.get.right.get.right.get.value == 31)
  assert (root.value == 20)
  
  
  println (split("(5(3()())(7()()))"))
 

}