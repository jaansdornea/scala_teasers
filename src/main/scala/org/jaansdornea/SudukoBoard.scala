package scala_questions

import scala.annotation.tailrec

/**
 * suduko
 */
object SudukoBoard extends App {

  object Square {

    val up: (Square) => Option[Square] = (square) => square.up()
    def down: (Square) => Option[Square] = (square) => square.down()
    def right: (Square) => Option[Square] = (square) => square.right()
    def left: (Square) => Option[Square] = (square) => square.left()

  }
  case class Square(row: Int, column: Int) {

    /**
     * general function for moving from one square to another
     * @return Some[Square] iff is valid move defined by isAble else None
     */
    def move(isAble: Boolean, rowDir: Int, columnDir: Int): Option[Square] =
      if (isAble) Some(Square(row + rowDir, column + columnDir))
      else None

    def up: () => Option[Square] = () => move((row > 0), -1, 0)
    def down: () => Option[Square] = () => move((row < 8), 1, 0)
    def right: () => Option[Square] = () => move((column < 8), 0, 1)
    def left: () => Option[Square] = () => move((column > 0), 0, -1)

  }

  type Board = Map[Square, Option[Int]]

  /**
   * a validator needs to
   * 1.) ensure that all numbers in validation set x >0 && x <10
   * 2.) ensure that no number is repeated
   */
  type Validator = ((Square, Board) => Boolean)

  @tailrec
  def check(start: Option[Square], stillValid: Boolean, move: (Square) => Option[Square], previouslyUsed: Set[Int], board: Board): Boolean = {
    if (!stillValid) false
    else start match {
      case None => /* off the board */ stillValid
      case Some(square) => {
        val (nextSquare, valid, cache) = board.get(square) match {
          case None => /* malformed board */ throw new IllegalArgumentException(s"board does not have square: $square")
          case Some(option) => {
            option match {
              /*board square hasn't yet been filled in with a value*/
              case None => (move(square), stillValid, previouslyUsed) //check (move(square), stillValid, move, previouslyUsed, board)
              case Some(value) => /* board has a value */ {
                if (previouslyUsed.contains(value)) {
                  (Some(square), false, previouslyUsed)
                } else {
                  (move(square), stillValid, previouslyUsed + value)
                }
              }
            }
          }
        }
        check(nextSquare, valid, move, cache, board)
      }
    }
  }

  /**
   * around once takes a stack of directions
   * returning a function when called will pop a direction off the stack
   * and move in that direction from the input square
   * 
   */
  /*def aroundOnce(stack: scala.collection.mutable.Stack[(Int, Int)]):(SudukoBoard.Square) => Option[SudukoBoard.Square] = { 
    (square) => 
        val canMove = !stack.isEmpty
        if (canMove) {
          val (rowDir, columnDir) = stack.pop
          val newPosition = square.move(canMove, rowDir, columnDir) 
          newPosition
        } else None
  }*/
  
  def aroundOnce(stack: scala.collection.mutable.Stack[(Square) => Option[Square]]): (SudukoBoard.Square) => Option[SudukoBoard.Square] = 
    (square) => 
        if (!stack.isEmpty) stack.pop.apply(square) 
        else None
  
  
  def getRotationStack(): scala.collection.mutable.Stack[(Square) => Option[Square]] = {
    import Square._
    val _12 = up
    val _1 = right // right of 12
    val _3 = down // down from 1
    val _5 = down // down from 3
    val _6 = left // left of 5
    val _8 = left // left of 6
    val _9 = up // up from 8
    val _10 = up // up from 9
    scala.collection.mutable.Stack(_12, _1, _3, _5, _6, _8, _9, _10)
  }
  
  val rowValidator: Validator = ((start, board) => { check(Some(start), true, Square.right, Set[Int](), board) })
  val columnValidator: Validator = ((start, board) => { check(Some(start), true, Square.down, Set[Int](), board) })
  def getQuadrantValidator(): Validator = ((start, board) => { check(Some(start), true, aroundOnce(getRotationStack())/*.apply()*/, Set[Int](), board) })

  // all locations to put into validators
  val quadrantValidatorStarts = Set(Square(1, 1), Square(1, 4), Square(1, 7), Square(4, 1), Square(4, 4), Square(4, 7), Square(7, 1), Square(7, 4), Square(7, 7))
  val rowValidatorStarts = (0 to 8).foldLeft(Set[Square]())((set, row) => set + Square(row, 0))
  val columnValidatorStarts = (0 to 8).foldLeft(Set[Square]())((set, column) => set + Square(0, column))

  val toRunValidationOn: List[(Validator, Set[Square])] =
    List((rowValidator, rowValidatorStarts), (columnValidator, columnValidatorStarts))
    
    // (quadrantValidator, quadrantValidatorStarts))

  def checkBoard(board: Board): Boolean = {
    val rowColumnValidationPairs: List[(SudukoBoard.Square, SudukoBoard.Validator)] = toRunValidationOn.flatMap { case (validator, squares) => squares.map((start) => (start, validator)) }
    val quadrantPairs: List[(SudukoBoard.Square, SudukoBoard.Validator)] = quadrantValidatorStarts.toList.map((start) => (start, getQuadrantValidator())) 
    val pairs = quadrantPairs ::: rowColumnValidationPairs  
    
    @tailrec
    def runCheck(stillValid: Boolean, rest: List[(SudukoBoard.Square, SudukoBoard.Validator)]): Boolean =
      if (!stillValid || rest.isEmpty) stillValid
      else {
        val (start, validator) = rest.head
        val partialCheck = validator.apply(start, board)
        runCheck(partialCheck, rest.tail)
      }

    runCheck(true, pairs)

  }

  def buildBoard(entries: (Int, Int, Int)*): Board =
    entries.foldLeft(Map[Square, Option[Int]]())((board, entry) => board + (Square(entry._1, entry._2) -> { if (entry._3 == -1) None else Some(entry._3) }))

  val validTestBoard =
    buildBoard(
      (0, 0, 5), (0, 1, 3), (0, 2, -1), (0, 3, -1), (0, 4, 7), (0, 5, -1), (0, 6, -1), (0, 7, -1), (0, 8, -1),
      (1, 0, 6), (1, 1, -1), (1, 2, -1), (1, 3, 1), (1, 4, 9), (1, 5, 5), (1, 6, -1), (1, 7, -1), (1, 8, -1),
      (2, 0, -1), (2, 1, 9), (2, 2, 8), (2, 3, -1), (2, 4, -1), (2, 5, -1), (2, 6, -1), (2, 7, 6), (2, 8, -1),

      (3, 0, 8), (3, 1, -1), (3, 2, -1), (3, 3, -1), (3, 4, 6), (3, 5, -1), (3, 6, -1), (3, 7, -1), (3, 8, 3),
      (4, 0, 4), (4, 1, -1), (4, 2, -1), (4, 3, 8), (4, 4, -1), (4, 5, 3), (4, 6, -1), (4, 7, -1), (4, 8, 1),
      (5, 0, 7), (5, 1, -1), (5, 2, -1), (5, 3, -1), (5, 4, 2), (5, 5, -1), (5, 6, -1), (5, 7, -1), (5, 8, 6),

      (6, 0, -1), (6, 1, 6), (6, 2, -1), (6, 3, -1), (6, 4, -1), (6, 5, -1), (6, 6, 2), (6, 7, 8), (6, 8, -1),
      (7, 0, -1), (7, 1, -1), (7, 2, -1), (7, 3, 4), (7, 4, 1), (7, 5, 9), (7, 6, -1), (7, 7, -1), (7, 8, 5),
      (8, 0, -1), (8, 1, -1), (8, 2, -1), (8, 3, -1), (8, 4, 8), (8, 5, -1), (8, 6, -1), (8, 7, 7), (8, 8, 9))

      
  val invalidRowBoard = 
    buildBoard(
      (0, 0, 5), (0, 1, 3), (0, 2, 3), (0, 3, -1), (0, 4, 7), (0, 5, -1), (0, 6, -1), (0, 7, -1), (0, 8, -1),
      (1, 0, 6), (1, 1, -1), (1, 2, -1), (1, 3, 1), (1, 4, 9), (1, 5, 5), (1, 6, -1), (1, 7, -1), (1, 8, -1),
      (2, 0, -1), (2, 1, 9), (2, 2, 8), (2, 3, -1), (2, 4, -1), (2, 5, -1), (2, 6, -1), (2, 7, 6), (2, 8, -1),

      (3, 0, 8), (3, 1, -1), (3, 2, -1), (3, 3, -1), (3, 4, 6), (3, 5, -1), (3, 6, -1), (3, 7, -1), (3, 8, 3),
      (4, 0, 4), (4, 1, -1), (4, 2, -1), (4, 3, 8), (4, 4, -1), (4, 5, 3), (4, 6, -1), (4, 7, -1), (4, 8, 1),
      (5, 0, 7), (5, 1, -1), (5, 2, -1), (5, 3, -1), (5, 4, 2), (5, 5, -1), (5, 6, -1), (5, 7, -1), (5, 8, 6),

      (6, 0, -1), (6, 1, 6), (6, 2, -1), (6, 3, -1), (6, 4, -1), (6, 5, -1), (6, 6, 2), (6, 7, 8), (6, 8, -1),
      (7, 0, -1), (7, 1, -1), (7, 2, -1), (7, 3, 4), (7, 4, 1), (7, 5, 9), (7, 6, -1), (7, 7, -1), (7, 8, 5),
      (8, 0, -1), (8, 1, -1), (8, 2, -1), (8, 3, -1), (8, 4, 8), (8, 5, -1), (8, 6, -1), (8, 7, 7), (8, 8, 9))

      
  val invalidColumnBoard = 
    buildBoard(
      (0, 0, 5), (0, 1, 3), (0, 2, -1), (0, 3, -1), (0, 4, 7), (0, 5, -1), (0, 6, -1), (0, 7, -1), (0, 8, -1),
      (1, 0, 6), (1, 1, -1), (1, 2, -1), (1, 3, 1), (1, 4, 9), (1, 5, 5), (1, 6, -1), (1, 7, -1), (1, 8, -1),
      (2, 0, -1), (2, 1, 9), (2, 2, 8), (2, 3, -1), (2, 4, -1), (2, 5, -1), (2, 6, -1), (2, 7, 6), (2, 8, -1),

      (3, 0, 8), (3, 1, -1), (3, 2, -1), (3, 3, -1), (3, 4, 6), (3, 5, -1), (3, 6, -1), (3, 7, -1), (3, 8, 3),
      (4, 0, 4), (4, 1, -1), (4, 2, -1), (4, 3, 8), (4, 4, -1), (4, 5, 3), (4, 6, -1), (4, 7, -1), (4, 8, 1),
      (5, 0, 7), (5, 1, -1), (5, 2, -1), (5, 3, -1), (5, 4, 2), (5, 5, -1), (5, 6, -1), (5, 7, -1), (5, 8, 6),

      (6, 0, -1), (6, 1, 6), (6, 2, -1), (6, 3, -1), (6, 4, -1), (6, 5, -1), (6, 6, 2), (6, 7, 8), (6, 8, -1),
      (7, 0, -1), (7, 1, 3), (7, 2, -1), (7, 3, 4), (7, 4, 1), (7, 5, 9), (7, 6, -1), (7, 7, -1), (7, 8, 5),
      (8, 0, -1), (8, 1, -1), (8, 2, -1), (8, 3, -1), (8, 4, 8), (8, 5, -1), (8, 6, -1), (8, 7, 7), (8, 8, 9))

      
  val invalidQuadrantBoard = 
    buildBoard(
      (0, 0, 5), (0, 1, 3), (0, 2, -1), (0, 3, -1), (0, 4, 7), (0, 5, -1), (0, 6, -1), (0, 7, -1), (0, 8, -1),
      (1, 0, 6), (1, 1, -1), (1, 2, -1), (1, 3, 1), (1, 4, 9), (1, 5, 5), (1, 6, -1), (1, 7, -1), (1, 8, -1),
      (2, 0, 3), (2, 1, 9), (2, 2, 8), (2, 3, -1), (2, 4, -1), (2, 5, -1), (2, 6, -1), (2, 7, 6), (2, 8, -1),

      (3, 0, 8), (3, 1, -1), (3, 2, -1), (3, 3, -1), (3, 4, 6), (3, 5, -1), (3, 6, -1), (3, 7, -1), (3, 8, 3),
      (4, 0, 4), (4, 1, -1), (4, 2, -1), (4, 3, 8), (4, 4, -1), (4, 5, 3), (4, 6, -1), (4, 7, -1), (4, 8, 1),
      (5, 0, 7), (5, 1, -1), (5, 2, -1), (5, 3, -1), (5, 4, 2), (5, 5, -1), (5, 6, -1), (5, 7, -1), (5, 8, 6),

      (6, 0, -1), (6, 1, 6), (6, 2, -1), (6, 3, -1), (6, 4, -1), (6, 5, -1), (6, 6, 2), (6, 7, 8), (6, 8, -1),
      (7, 0, -1), (7, 1, -1), (7, 2, -1), (7, 3, 4), (7, 4, 1), (7, 5, 9), (7, 6, -1), (7, 7, -1), (7, 8, 5),
      (8, 0, -1), (8, 1, -1), (8, 2, -1), (8, 3, -1), (8, 4, 8), (8, 5, -1), (8, 6, -1), (8, 7, 7), (8, 8, 9))

      
  assert(checkBoard(validTestBoard))
  assert(!checkBoard(invalidRowBoard))
  assert(!checkBoard(invalidColumnBoard))
  assert(!checkBoard(invalidQuadrantBoard))

}