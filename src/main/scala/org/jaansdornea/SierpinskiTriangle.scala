package org.jaansdornea

import scala.annotation.tailrec

object SierpinskiTriangle extends App {

  case class Index(val row: Int, val column: Int)

  val initialTop = Index(0, 62 / 2)
  val bottomLeft = Index(31, 0)
  val bottomRight = Index(31, 62)

  // create and initialize the board
  val board: Array[Array[Boolean]] = Array.ofDim[Boolean](32, 63)
  0 to 31 foreach { row =>
    0 to 62 foreach { column =>
      board(row)(column) = true
    }
  }

  def foldTop(top: Index): (Index, Index, Index) = {
    @tailrec
    def getBottom(top: Index): Index =
      if (top.row == 31) {
        drawBoard(0)
        top
      } else if (board(top.row)(top.column)) {
        drawBoard(0)
        Index(top.row -1, top.column)
      } else getBottom(Index(top.row + 1, top.column))

    val bottomOfTopTriangle = getBottom(top)
    val newBottomOfTopTriangle = Index((bottomOfTopTriangle.row - top.row) / 2, top.column)

    ((newBottomOfTopTriangle.row + 1) to (bottomOfTopTriangle.row)).foreach((row) => {
      drawBoard(0)
      val columnOffset = bottomOfTopTriangle.row - row
      ((top.column - columnOffset) to (top.column + columnOffset)).foreach((column) => {
        board(row)(column) = !board(row)(column)
        drawBoard(0)
        println("")
      })
    })
    
    val heads = (top, Index(newBottomOfTopTriangle.row, top.column - (bottomOfTopTriangle.row - newBottomOfTopTriangle.row)),
      Index(newBottomOfTopTriangle.row, top.column + (bottomOfTopTriangle.row - newBottomOfTopTriangle.row)))
    println(heads)
    heads
  }

  def flipTriangle(triangle: (Index, Index, Index)): Unit = {
    val (x, y, z) = triangle
    val (parallel, perpindicular) = {
      if (x.row == y.row)
        if (x.column < y.column) ((x, y), z)
        else ((y, x), z)
      else if (x.row == z.row)
        if (x.column < z.column) ((x, z), y)
        else ((z, x), y)
      else if (y.column < z.column) ((y, z), x)
      else ((z, y), x)
    }
    if (parallel._1.row < perpindicular.row) { // upside down  
      (0 to (perpindicular.row - parallel._1.row)).foreach((offset) =>
        new Range(parallel._1.column + offset, parallel._2.column - offset + 1, 1).foreach((column) =>
          board(parallel._1.row + offset)(column) = !(board(parallel._1.row + offset)(column))))
    } else {
      (0 to (parallel._1.row - perpindicular.row)).foreach((offset) =>
        new Range(parallel._1.column + offset, parallel._2.column - offset + 1, 1).foreach((column) =>
          board(parallel._1.row - offset)(column) = !(board(parallel._1.row - offset)(column))))
    }
  }

  def flipIterations(iteration: Int, heads: (Index, Index, Index)): Unit =
    if (iteration == 0) return
    else
      List(heads._1, heads._2, heads._3).foreach { head => flipIterations(iteration - 1, foldTop(head)) }

  def drawBoard(iterations: Int): Unit = {
    if (iterations > 0) flipTriangle(initialTop, bottomLeft, bottomRight)
    if (iterations > 1) {
      val heads = foldTop(initialTop)
      flipIterations(iterations - 2, heads)
    }

    0 to 31 foreach { row =>
      0 to 62 foreach { column =>
        print(if (board(row)(column)) "_" else "1")
      }
      print('\n')
    }
  }

  drawBoard(3)

}